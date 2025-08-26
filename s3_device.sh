#!/bin/bash
set -e

echo "[*] starting the ~s3@1.0 device"

# start minio if not running
echo "[*] checking minio status"
cd minio-cluster
if ! sudo docker compose ps | grep -q "Up"; then
    echo "[*] starting minio cluster"
    sudo docker compose up -d
    echo "[*] waiting for minio"
    sleep 3
else
    echo "[*] minio already running"
fi
cd ..

# build the nif crate
echo "[*] building s3_nif device"
./build.sh

# start the hyperbeam service (server related config)
# echo "[*] starting hyperbeam server"
# rebar3 shell
