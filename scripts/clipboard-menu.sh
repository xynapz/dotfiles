#!/usr/bin/env bash
# Clipboard menu using fuzzel + cliphist
source "$(dirname "$0")/bemenu-theme.sh"
cliphist list | bemenu --prompt "Clipboard: " | cliphist decode | wl-copy
