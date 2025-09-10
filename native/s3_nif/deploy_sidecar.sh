#!/usr/bin/env bash
set -e

REPO_ROOT="$(cd "$(dirname "$0")" && pwd)"
CARGO_DIR="$REPO_ROOT/native/s3_nif"
BIN_NAME="sidecar"
INSTALL_DIR="/opt/s3_sidecar"
BIN_PATH="$CARGO_DIR/target/release/$BIN_NAME"
TARGET_BIN="$INSTALL_DIR/s3_sidecar"

echo "[deploy] building $BIN_NAME..."
cd "$CARGO_DIR"
cargo build --release --bin "$BIN_NAME"

echo "[deploy] installing to $TARGET_BIN..."
sudo mkdir -p "$INSTALL_DIR"
sudo install -Dm755 "$BIN_PATH" "$TARGET_BIN"

echo "[deploy] reloading systemd..."
sudo systemctl daemon-reload
sudo systemctl restart s3_sidecar.service
sudo systemctl status --no-pager s3_sidecar.service

echo "[deploy] done."