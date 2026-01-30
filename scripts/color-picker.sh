#!/usr/bin/env bash
# Color picker for Sway/Wayland
# Uses grim + slurp + imagemagick to pick a color
# Usage: ./color-picker.sh [--rgb|--hsl|--hex]

set -euo pipefail

FORMAT="${1:-hex}"

# Take a screenshot of a single pixel at the cursor position
PIXEL_INFO=$(grim -g "$(slurp -p)" -t ppm - 2>/dev/null | magick - -format '%[pixel:p{0,0}]' info:-)

# Extract RGB values
RGB=$(echo "$PIXEL_INFO" | grep -oP 'srgb\(\K[^)]+')
R=$(echo "$RGB" | cut -d',' -f1 | tr -d ' ')
G=$(echo "$RGB" | cut -d',' -f2 | tr -d ' ')
B=$(echo "$RGB" | cut -d',' -f3 | tr -d ' ')

# Convert to 0-255 range if needed (ImageMagick might return percentages)
if [[ "$R" == *"%" ]]; then
    R=$(echo "${R%\%} * 255 / 100" | bc)
    G=$(echo "${G%\%} * 255 / 100" | bc)
    B=$(echo "${B%\%} * 255 / 100" | bc)
fi

# Format output
case "$FORMAT" in
    --rgb|rgb)
        COLOR="rgb($R, $G, $B)"
        ;;
    --hsl|hsl)
        # Convert RGB to HSL using awk
        COLOR=$(awk -v r="$R" -v g="$G" -v b="$B" 'BEGIN {
            r = r/255; g = g/255; b = b/255;
            max = (r > g) ? r : g; max = (max > b) ? max : b;
            min = (r < g) ? r : g; min = (min < b) ? min : b;
            l = (max + min) / 2;
            if (max == min) { h = s = 0; }
            else {
                d = max - min;
                s = (l > 0.5) ? d / (2 - max - min) : d / (max + min);
                if (max == r) { h = (g - b) / d + (g < b ? 6 : 0); }
                else if (max == g) { h = (b - r) / d + 2; }
                else { h = (r - g) / d + 4; }
                h = h / 6;
            }
            printf "hsl(%.0f, %.0f%%, %.0f%%)", h*360, s*100, l*100;
        }')
        ;;
    --hex|hex|*)
        # Default to hex
        COLOR=$(printf "#%02x%02x%02x" "$R" "$G" "$B")
        ;;
esac

# Only proceed if we got a color
if [ -n "$COLOR" ] && [ "$COLOR" != "null" ]; then
    echo -n "$COLOR" | wl-copy
    notify-send "Color Picked" "$COLOR" -t 3000
fi
