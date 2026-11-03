# OpenClaw environment

Build the image from this directory with `./build`, then start an interactive
shell with `./run`. Run `openclaw onboard` the first time and
`openclaw-start` to start the gateway and virtual display.

The image extends `igorhvr/bedlam-ubuntu` with Chromium, Xvfb, OpenClaw,
GitHub CLI, Homebrew, and the Gemini, Codex, and Claude CLIs. Builds install
the current releases of these tools; this is not a version-locked image.

Configuration and sessions persist in `~/.openclaw` on the host. Set
`OPENCLAW_STATE_DIR` to use another host directory. Treat that directory
as private because it can contain credentials and sessions. The gateway
ports 18789 and 18793 bind to host loopback only. The container is removed
on exit; the mounted state directory is retained.

The default run needs no privileged container. If tunnel support is needed
on Linux, set `OPENCLAW_ENABLE_TUN=1` to expose `/dev/net/tun` and grant
`NET_ADMIN`. Otherwise those permissions are absent.

Set `CONTAINER_ENGINE` to another compatible engine executable (for example,
`podman`), `OPENCLAW_IMAGE` to change the image tag, or
`OPENCLAW_CONTAINER_NAME` to run under another container name.
