#!/bin/sh

# ---------------- System Services -----------------
export SSH_ASKPASS=/usr/bin/ksshaskpass
export DISPLAY=:0
export SSH_ASKPASS_REQUIRE=prefer
# Set a random wallpaper on startup
feh --bg-scale "$(find ~/Pictures/wall -type f \( -iname '*.jpg' -o -iname '*.png' \) | shuf -n 1)"

picom -b

dunst & #notification daemon

dwmblocks &

alacritty &

nm-applet &

