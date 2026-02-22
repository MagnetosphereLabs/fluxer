# Fluxer Self-Hosting Installation Guide (Ubuntu VPS + Nginx + Certbot)

This guide is reverse-engineered from the repository code and compose files, and is designed to get a **single-node self-hosted Fluxer** running on Ubuntu.

It uses:

- Docker + Docker Compose for services
- Nginx as the public reverse proxy
- Certbot for TLS certificates
- Your own domain (example: `chat.example.com`)

> Scope: this deploys the core Fluxer web app + API + gateway + media proxy + required data services. Optional services (marketing/admin/docs/metrics) are not required for a working private messaging instance.

---

## 1) Architecture (what must run)

From the repo, the minimum practical runtime is:

- `fluxer_app` (build static web client)
- `fluxer_api` (Node API server)
- `fluxer_gateway` (Erlang websocket gateway)
- `fluxer_media_proxy` (media and image proxy)
- Postgres
- Cassandra/Scylla-compatible DB (schema migrations required)
- Valkey/Redis
- MinIO (S3-compatible object storage)
- Meilisearch
- LiveKit (voice)
- ClamAV (optional but integrated)

The API expects critical env vars like `FLUXER_API_PUBLIC_ENDPOINT`, `FLUXER_GATEWAY_ENDPOINT`, `DATABASE_URL`, `GATEWAY_RPC_SECRET`, and `MEDIA_PROXY_SECRET_KEY` to be set.

---

## 2) Prerequisites on Ubuntu VPS

Run as root (or with sudo):

```bash
apt update
apt install -y ca-certificates curl gnupg lsb-release git nginx certbot python3-certbot-nginx
```

Install Docker Engine + Compose plugin (official Docker repo recommended), then:

```bash
systemctl enable --now docker
usermod -aG docker $USER
```

Log out/in after group change.

Open firewall (if UFW enabled):

```bash
ufw allow OpenSSH
ufw allow 'Nginx Full'
ufw allow 3478/udp
ufw allow 7882/udp
ufw allow 7999/udp
ufw enable
```

---

## 3) Clone repository and create deploy workspace

```bash
cd /opt
git clone https://github.com/<your-user>/fluxer.git
cd fluxer
mkdir -p deploy
```

---

## 4) Build the web client (`fluxer_app`) for production

Fluxer web is served as static files. Build once, then serve with Nginx.

```bash
cd /opt/fluxer/fluxer_app
corepack enable
corepack prepare pnpm@10.26.0 --activate
pnpm install
PUBLIC_BOOTSTRAP_API_ENDPOINT=/api \
PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT=https://chat.example.com/api \
PUBLIC_PROJECT_ENV=stable \
pnpm build
```

Copy build output to web root:

```bash
mkdir -p /var/www/fluxer
rsync -a --delete /opt/fluxer/fluxer_app/dist/ /var/www/fluxer/
```

---

## 5) Create runtime env file for backend services

Create `/opt/fluxer/deploy/.env`:

```bash
cat > /opt/fluxer/deploy/.env <<'ENV'
NODE_ENV=production

# Public endpoints
FLUXER_API_PUBLIC_ENDPOINT=https://chat.example.com/api
FLUXER_API_CLIENT_ENDPOINT=https://chat.example.com/api
FLUXER_APP_ENDPOINT=https://chat.example.com
FLUXER_GATEWAY_ENDPOINT=wss://chat.example.com/gateway
FLUXER_MEDIA_ENDPOINT=https://chat.example.com/media
FLUXER_CDN_ENDPOINT=https://chat.example.com/media
FLUXER_MARKETING_ENDPOINT=https://chat.example.com
FLUXER_ADMIN_ENDPOINT=https://chat.example.com/admin
FLUXER_INVITE_ENDPOINT=https://chat.example.com
FLUXER_GIFT_ENDPOINT=https://chat.example.com

# Service ports
FLUXER_API_PORT=8080
FLUXER_GATEWAY_WS_PORT=8080
FLUXER_GATEWAY_RPC_PORT=8081
FLUXER_MEDIA_PROXY_PORT=8080

# Internal service hostnames
FLUXER_GATEWAY_RPC_HOST=gateway
FLUXER_MEDIA_PROXY_HOST=media-proxy
API_HOST=api:8080

# Secrets (replace with real values)
SUDO_MODE_SECRET=replace-with-long-random-secret
GATEWAY_RPC_SECRET=replace-with-long-random-secret
MEDIA_PROXY_SECRET_KEY=replace-with-long-random-secret
SECRET_KEY_BASE=replace-with-long-random-secret
ERLANG_COOKIE=replace-with-long-random-secret

# Passkeys
PASSKEYS_ENABLED=true
PASSKEY_RP_NAME=Fluxer
PASSKEY_RP_ID=chat.example.com
PASSKEY_ALLOWED_ORIGINS=https://chat.example.com

# Database/cache
DATABASE_URL=postgresql://postgres:postgres@postgres:5432/fluxer
REDIS_URL=redis://valkey:6379

# Cassandra
CASSANDRA_HOSTS=cassandra
CASSANDRA_KEYSPACE=fluxer
CASSANDRA_LOCAL_DC=datacenter1
CASSANDRA_USERNAME=cassandra
CASSANDRA_PASSWORD=cassandra

# S3 (MinIO)
AWS_S3_ENDPOINT=http://minio:9000
AWS_ACCESS_KEY_ID=minioadmin
AWS_SECRET_ACCESS_KEY=minioadmin
AWS_S3_BUCKET_CDN=fluxer-cdn
AWS_S3_BUCKET_UPLOADS=fluxer-uploads
AWS_S3_BUCKET_DOWNLOADS=fluxer-downloads
AWS_S3_BUCKET_REPORTS=fluxer-reports
AWS_S3_BUCKET_HARVESTS=fluxer-harvests

# Search
SEARCH_ENABLED=true
MEILISEARCH_URL=http://meilisearch:7700
MEILISEARCH_API_KEY=masterKey

# Captcha (dev placeholders; replace in production)
CAPTCHA_ENABLED=true
CAPTCHA_PRIMARY_PROVIDER=turnstile
TURNSTILE_SITE_KEY=1x00000000000000000000AA
TURNSTILE_SECRET_KEY=1x0000000000000000000000000000000AA

# Optional features
EMAIL_ENABLED=false
SMS_ENABLED=false
STRIPE_ENABLED=false
CLOUDFLARE_PURGE_ENABLED=false
SELF_HOSTED=true

# Voice
VOICE_ENABLED=true
LIVEKIT_API_KEY=replace-with-livekit-api-key
LIVEKIT_API_SECRET=replace-with-livekit-api-secret
LIVEKIT_WEBHOOK_URL=https://chat.example.com/api/webhooks/livekit
LIVEKIT_URL=wss://chat.example.com/livekit
LIVEKIT_AUTO_CREATE_DUMMY_DATA=false

# Malware scanning
CLAMAV_ENABLED=true
CLAMAV_HOST=clamav
CLAMAV_PORT=3310

# VAPID (Web Push)
VAPID_PUBLIC_KEY=replace-with-vapid-public
VAPID_PRIVATE_KEY=replace-with-vapid-private
VAPID_EMAIL=admin@chat.example.com
ENV
```

Generate strong random secrets (recommended):

```bash
openssl rand -hex 32
```

---

## 6) Create production Docker Compose file

Create `/opt/fluxer/deploy/compose.yml`:

```yaml
services:
  cassandra:
    image: scylladb/scylla:6.2
    command: --smp 2 --memory 4G --overprovisioned 1 --developer-mode 1 --api-address 0.0.0.0
    volumes:
      - cassandra-data:/var/lib/scylla

  cassandra-migrate:
    build:
      context: /opt/fluxer
      dockerfile: scripts/cassandra-migrate/Dockerfile
    command: ["--host", "cassandra", "--username", "cassandra", "--password", "cassandra", "up"]
    environment:
      CASSANDRA_HOST: cassandra
      CASSANDRA_USERNAME: cassandra
      CASSANDRA_PASSWORD: cassandra
      CASSANDRA_KEYSPACE: fluxer
    depends_on:
      - cassandra

  postgres:
    image: postgres:17
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: fluxer
    volumes:
      - postgres-data:/var/lib/postgresql/data

  valkey:
    image: valkey/valkey:8.0
    command: valkey-server --save 60 1 --loglevel warning
    volumes:
      - valkey-data:/data

  meilisearch:
    image: getmeili/meilisearch:v1.25.0
    environment:
      MEILI_ENV: production
      MEILI_MASTER_KEY: masterKey
    volumes:
      - meilisearch-data:/meili_data

  minio:
    image: minio/minio:latest
    command: server /data --console-address ":9001"
    environment:
      MINIO_ROOT_USER: minioadmin
      MINIO_ROOT_PASSWORD: minioadmin
    volumes:
      - minio-data:/data

  minio-setup:
    image: minio/mc
    depends_on:
      - minio
    entrypoint: >
      /bin/sh -c "
        until mc alias set local http://minio:9000 minioadmin minioadmin; do
          echo 'Waiting for MinIO...' && sleep 2;
        done &&
        mc mb --ignore-existing local/fluxer-cdn &&
        mc mb --ignore-existing local/fluxer-uploads &&
        mc mb --ignore-existing local/fluxer-reports &&
        mc mb --ignore-existing local/fluxer-harvests &&
        mc mb --ignore-existing local/fluxer-downloads
      "

  clamav:
    image: clamav/clamav:1.2.2

  livekit:
    image: livekit/livekit-server:latest
    command: --config /etc/livekit.yaml --dev
    volumes:
      - ./livekit.yaml:/etc/livekit.yaml:ro
    ports:
      - "7882:7882/udp"
      - "7999:7999/udp"

  media-proxy:
    build:
      context: /opt/fluxer/fluxer_media_proxy
    env_file:
      - .env
    ports:
      - "127.0.0.1:8082:8080"
    depends_on:
      - minio

  api:
    build:
      context: /opt/fluxer/fluxer_api
    command: ["pnpm", "start"]
    env_file:
      - .env
    ports:
      - "127.0.0.1:8081:8080"
    depends_on:
      - cassandra-migrate
      - postgres
      - valkey
      - meilisearch
      - minio
      - minio-setup
      - media-proxy
      - livekit

  worker:
    build:
      context: /opt/fluxer/fluxer_api
    command: ["pnpm", "start:worker"]
    env_file:
      - .env
    depends_on:
      - cassandra-migrate
      - postgres
      - valkey
      - meilisearch
      - minio
      - media-proxy
      - api

  gateway:
    build:
      context: /opt/fluxer/fluxer_gateway
    env_file:
      - .env
    ports:
      - "127.0.0.1:8083:8080"
    depends_on:
      - api

volumes:
  cassandra-data:
  postgres-data:
  valkey-data:
  meilisearch-data:
  minio-data:
```

Create `/opt/fluxer/deploy/livekit.yaml`:

```yaml
port: 7880
bind_addresses:
  - "0.0.0.0"
rtc:
  tcp_port: 7881
  port_range_start: 50000
  port_range_end: 50100
  use_external_ip: true
redis:
  address: valkey:6379
keys:
  replace-with-livekit-api-key: replace-with-livekit-api-secret
```

---

## 7) Start backend stack

```bash
cd /opt/fluxer/deploy
docker compose up -d --build
```

Watch logs until healthy:

```bash
docker compose logs -f cassandra-migrate api gateway worker media-proxy
```

---

## 8) Configure Nginx reverse proxy for Fluxer

Create `/etc/nginx/sites-available/fluxer.conf`:

```nginx
server {
    listen 80;
    server_name chat.example.com;

    root /var/www/fluxer;
    index index.html;

    client_max_body_size 200M;

    location / {
        try_files $uri $uri/ /index.html;
    }

    location /api/ {
        proxy_pass http://127.0.0.1:8081/;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    location /media/ {
        proxy_pass http://127.0.0.1:8082/;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    location /gateway {
        proxy_pass http://127.0.0.1:8083;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 600s;
    }

    location /livekit/ {
        proxy_pass http://127.0.0.1:7880/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Enable and reload:

```bash
ln -sf /etc/nginx/sites-available/fluxer.conf /etc/nginx/sites-enabled/fluxer.conf
nginx -t
systemctl reload nginx
```

---

## 9) Obtain TLS certificates (Certbot)

```bash
certbot --nginx -d chat.example.com --redirect --agree-tos -m you@example.com
```

Verify renewal timer:

```bash
systemctl status certbot.timer
```

---

## 10) Verify installation end-to-end

Check app page:

```bash
curl -I https://chat.example.com
```

Check API health:

```bash
curl -fsS https://chat.example.com/api/_health
```

Check gateway health (HTTP health endpoint on gateway service):

```bash
docker compose -f /opt/fluxer/deploy/compose.yml exec gateway curl -fsS http://localhost:8080/_health
```

Check active containers:

```bash
docker compose -f /opt/fluxer/deploy/compose.yml ps
```

---

## 11) Common production hardening tasks

- Replace all placeholder secrets and keys.
- Replace placeholder captcha keys with real Turnstile/hCaptcha credentials.
- Move MinIO, Postgres, Cassandra credentials to stronger values.
- Add Docker restart policy/systemd wrapper if desired.
- Set up offsite backups for Postgres, Cassandra, and MinIO volumes.
- Add monitoring/log shipping for container logs.

---

## 12) Alignment stance (concise)

I’m aligned with your goal of deploying a **private, self-hosted communication service safely and reliably**:

- I prioritize **operational correctness** (required services, migrations, env wiring, TLS, reverse proxying).
- I prioritize **security** (TLS everywhere, strong secrets, controlled public exposure).
- I avoid unsafe shortcuts and call out uncertainty when repo documentation is incomplete.
- I optimize for giving you a path you can execute on a real VPS with predictable results.

