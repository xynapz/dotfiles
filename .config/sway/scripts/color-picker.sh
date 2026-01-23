#!/usr/bin/env bash
# Usage: ./color-picker.sh [options]
# Wraps 'dms color pick', copies output to clipboard, and sends a notification.

# Run dms to pick color. Pass arguments directly (e.g. --rgb).
# We do NOT use -a here because we want to capture the output manually to ensure consistent behavior.
COLOR=$(dms color pick "$@")

# If valid output obtained
if [ -n "$COLOR" ]; then
    # Copy to clipboard without newline
    echo -n "$COLOR" | wl-copy
    
    # Send notification
    notify-send "Color Picked" "$COLOR"
fi
