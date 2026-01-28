#!/usr/bin/env bash
# Usage: ./color-picker.sh [--rgb|--hsl|--hsv|--cmyk]
# Picks a color using dms, copies to clipboard, and sends notification.

# Parse format argument (default: hex)
FORMAT="hex"
case "$1" in
    --rgb)  FORMAT="rgb" ;;
    --hsl)  FORMAT="hsl" ;;
    --hsv)  FORMAT="hsv" ;;
    --cmyk) FORMAT="cmyk" ;;
esac

# Use --json for clean output (no ANSI codes)
JSON=$(dms color pick --json)

# Exit if cancelled
[ -z "$JSON" ] && exit 0

# Extract the requested format using jq
COLOR=$(echo "$JSON" | jq -r ".$FORMAT")

if [ -n "$COLOR" ] && [ "$COLOR" != "null" ]; then
    echo -n "$COLOR" | wl-copy
    notify-send "Color Picked" "$COLOR"
fi
