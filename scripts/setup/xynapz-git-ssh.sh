#!/bin/bash

LOG_FILE="$HOME/.cache/xynapz-ssh.log"

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${BLUE}[INFO]${NC} $1"; echo "[INFO] $1" >> "$LOG_FILE"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; echo "[OK] $1" >> "$LOG_FILE"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; echo "[ERROR] $1" >> "$LOG_FILE"; exit 1; }

setup_github_ssh() {
    log "Setting up GitHub SSH Authentication..."
    SSH_KEY="$HOME/.ssh/id_ed25519"

    if [ ! -f "$SSH_KEY" ]; then
        log "Generating new Ed25519 SSH key..."
        ssh-keygen -t ed25519 -C "angeldhakal97@gmail.com" -f "$SSH_KEY"
    else
        log "SSH Key found. Skipping generation."
    fi

    # Add to Agent (Current Session)
    eval "$(ssh-agent -s)" > /dev/null
    ssh-add "$SSH_KEY" 2>/dev/null

    # 3. Copy to Clipboard
    if command -v wl-copy &> /dev/null; then
        cat "${SSH_KEY}.pub" | wl-copy
        success "Public key copied to clipboard!"
    fi

    # Display & Pause
    echo -e "${YELLOW}=====================================================${NC}"
    echo -e "${YELLOW}  ACTION REQUIRED: ADD KEY TO GITHUB IF NOT ALREADY ${NC}"
    echo -e "${YELLOW}=====================================================${NC}"

    log "Testing GitHub connection..."
    if ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
        success "GitHub Authentication Verified!"
    else
        echo -e "${RED}[WARN] GitHub authentication failed. Please check your key.${NC}"
        sleep 3
    fi
}

setup_github_ssh
