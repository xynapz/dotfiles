#!/usr/bin/env bash
# Clipboard menu using fuzzel + cliphist
# Replaces DMS clipboard toggle

set -euo pipefail

# Get selection from cliphist and decode it to wl-copy
cliphist list | fuzzel -d -p "Clipboard: " -w 60 -l 15 | cliphist decode | wl-copy
