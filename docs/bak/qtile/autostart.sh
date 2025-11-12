#!/bin/bash

# Autostart script for Qtile
# Make this file executable: chmod +x ~/.config/qtile/autostart.sh

# Set a random wallpaper on startup
feh --bg-scale "$(find ~/Pictures/wall -type f \( -iname '*.jpg' -o -iname '*.png' \) | shuf -n 1)"

# Compositor for transparency and effects
picom --config ~/.config/picom/picom.conf &

# Notification daemon
dunst &

# Network manager applet
nm-applet &

# Bluetooth applet
blueman-applet &

# Battery applet
# cbatticon &

# Volume control applet
pasystray &

# Set cursor theme
xsetroot -cursor_name left_ptr &

# Disable screen saver
xset s off &
xset -dpms &

# Set keyboard repeat rate
xset r rate 400 40 &

# Start emacs daemon
emacs --daemon &

# Polkit agent for authentication
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Auto-lock screen after 10 minutes (install i3lock: sudo pacman -S i3lock)
# xss-lock -- i3lock -c 000000 &

# Custom startup applications
# Add your own startup applications here
