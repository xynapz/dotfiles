#!/usr/bin/env bash
# Color picker using grim + slurp + imagemagick
FORMAT="${1:-hex}"
PIXEL_INFO=$(grim -g "$(slurp -p)" -t ppm - 2>/dev/null | magick - -format '%[pixel:p{0,0}]' info:-)
RGB=$(echo "$PIXEL_INFO" | grep -oP 'srgb\(\K[^)]+')
R=$(echo "$RGB" | cut -d',' -f1 | tr -d ' ')
G=$(echo "$RGB" | cut -d',' -f2 | tr -d ' ')
B=$(echo "$RGB" | cut -d',' -f3 | tr -d ' ')
[[ "$R" == *"%" ]] && { R=$(echo "${R%\%} * 255 / 100" | bc); G=$(echo "${G%\%} * 255 / 100" | bc); B=$(echo "${B%\%} * 255 / 100" | bc); }
case "$FORMAT" in
    --rgb|rgb) COLOR="rgb($R, $G, $B)" ;;
    *) COLOR=$(printf "#%02x%02x%02x" "$R" "$G" "$B") ;;
esac
[ -n "$COLOR" ] && { echo -n "$COLOR" | wl-copy; notify-send "Color Picked" "$COLOR" -t 3000; }
