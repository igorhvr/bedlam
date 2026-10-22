# syntax=docker/dockerfile:1.7
FROM igorhvr/bedlam-ubuntu
LABEL maintainer="Felipe Micaroni Lalli <bedlam@nok.uy>"

ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# -------------------------------
# System deps (headless + build tools)
#   Bedlam already includes: zsh, curl, git, file, procps, sudo, locales, dialog,
#   gnupg, lsb-release, unzip, xz-utils, ca-certificates, nodejs/npm, etc.
# -------------------------------
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
    build-essential \
    python3 python3-dev python3-pip \
    chromium xvfb \
    libnss3 libatk-bridge2.0-0 libgtk-3-0 libgbm1 libasound2t64 \
    ; \
    rm -rf /var/lib/apt/lists/*

# -------------------------------
# Install GitHub CLI (gh)
# -------------------------------
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    set -eux; \
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg; \
    chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg; \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends gh; \
    rm -rf /var/lib/apt/lists/*; \
    gh --version

# -------------------------------
# Homebrew (Linuxbrew) – manual, Docker-safe
# -------------------------------
RUN set -eux; \
    useradd -m -d /home/linuxbrew -s /bin/bash linuxbrew; \
    mkdir -p /home/linuxbrew/.linuxbrew; \
    chown -R linuxbrew:linuxbrew /home/linuxbrew

USER linuxbrew
ENV HOME=/home/linuxbrew

RUN set -eux; \
    git clone --depth=1 https://github.com/Homebrew/brew /home/linuxbrew/.linuxbrew/Homebrew; \
    mkdir -p /home/linuxbrew/.linuxbrew/bin; \
    ln -sf ../Homebrew/bin/brew /home/linuxbrew/.linuxbrew/bin/brew; \
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"; \
    brew --version

USER root
ENV HOME=/root

# Make brew usable in shells that *do* load /etc/profile.d
RUN set -eux; \
    echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' > /etc/profile.d/brew.sh

# Keep env for non-shell callers too (but note Bedlam may reset PATH inside zshrc)
ENV PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:${PATH}"
ENV HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
ENV HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar"
ENV HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew"

# -------------------------------
# Fix Bedlam PATH reset for root zsh
#   Bedlam sets: export PATH=/usr/local/sbin:...:/bin (drops brew)
#   We re-add Linuxbrew safely (no duplication).
# -------------------------------
RUN set -eux; \
    cat <<'EOF' >> /root/.zshrc
# Ensure Linuxbrew is available (Bedlam resets PATH above)
case ":$PATH:" in
  *":/home/linuxbrew/.linuxbrew/bin:"*) ;;
  *) export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH" ;;
esac
EOF

# -------------------------------
# npm reliability settings
#   Bedlam already installs nodejs/npm, so only configure.
# -------------------------------
RUN set -eux; \
    npm config set fund false; \
    npm config set audit false; \
    npm config set update-notifier false; \
    npm config set progress false; \
    npm config set fetch-retries 5; \
    npm config set fetch-retry-mintimeout 20000; \
    npm config set fetch-retry-maxtimeout 120000

# -------------------------------
# Tooling installs (cached)
# -------------------------------
ENV OPENCLAW_STATE_DIR=/root/.openclaw

RUN --mount=type=cache,target=/root/.npm \
    set -eux; \
    npm install -g openclaw@latest --no-audit --no-fund; \
    npm install -g @google/gemini-cli --no-audit --no-fund; \
    npm install -g @openai/codex --no-audit --no-fund; \
    npm install -g @anthropic-ai/claude-code --no-audit --no-fund

# -------------------------------
# Headless defaults
# -------------------------------
ENV CHROME_FLAGS="--window-size=1920,1080 --disable-gpu --no-sandbox"

# -------------------------------
# Copy OpenClaw start script
# -------------------------------
COPY openclaw-start /bin/openclaw-start
RUN chmod 0755 /bin/openclaw-start

# -------------------------------
# OpenClaw banner (do NOT overwrite Bedlam zshrc; append)
# -------------------------------
RUN set -eux; \
    cat <<'EOF' >> /etc/zsh/zshrc

# ---- OpenClaw banner (interactive only) ----
if [[ -o interactive ]]; then
  echo
  echo "OpenClaw quickstart:"
  echo "  Onboard (first run):        openclaw onboard"
  echo "  Start gateway:             openclaw-start"
  echo "  Gateway UI:                http://127.0.0.1:18789/"
  echo
  echo "Diagnostics & config:"
  echo "  Environment check:         openclaw doctor"
  echo "  Edit configuration:        openclaw configure"
  echo "  Show current config:       openclaw config show"
  echo
  echo "Agents & context:"
  echo "  List agents:               openclaw agent list"
  echo "  Use agent:                 openclaw agent use <name>"
  echo "  Current agent:             openclaw agent current"
  echo "  List loaded context:       openclaw context list"
  echo
  echo "  Help:                      openclaw help"
  echo
fi
EOF

# -------------------------------
# Build-time sanity check: brew must be visible in interactive zsh
# -------------------------------
RUN set -eux; \
    zsh -ic 'command -v brew >/dev/null && brew --version >/dev/null'

CMD ["/bin/zsh"]
