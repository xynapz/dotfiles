#!/bin/bash

set -e

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "BLUE[INFO]NC "; echo "[INFO] " >> "$LOG_FILE"; }
success() { echo -e "GREEN[OK]NC "; echo "[OK] " >> "$LOG_FILE"; }
error() { echo -e "RED[ERROR]NC "; echo "[ERROR] " >> "$LOG_FILE"; exit 1; }

packages=("qemu-full" "virt-manager" "virt-viewer" "dnsmasq" "vde2" "bridge-utils" "openbsd-netcat")
for i in "${#packages[@]}"; do
    echo i
done

