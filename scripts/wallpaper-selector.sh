#!/usr/bin/env bash

# Directory containing wallpapers
WALLPAPER_DIR="$HOME/Pictures/Wallpapers"
# Persistence file
CURRENT_WALLPAPER="$HOME/.current-wallpaper"

# Check if directory exists
if [ ! -d "$WALLPAPER_DIR" ]; then
    notify-send "Wallpaper Error" "Directory $WALLPAPER_DIR does not exist."
    exit 1
fi

# List files and use fuzzel to select one
SELECTED=$(ls "$WALLPAPER_DIR" | fuzzel -d -p "Wallpaper: ")

if [ -n "$SELECTED" ]; then
    FULL_PATH="$WALLPAPER_DIR/$SELECTED"
    
    # Update sway background
    swaymsg output "*" bg "$FULL_PATH" fill
    
    # Update persistence symlink
    ln -sf "$FULL_PATH" "$CURRENT_WALLPAPER"
    
    # Optional: Notify
    notify-send "Wallpaper Changed" "Applied: $SELECTED" || true
    
    # Optional: Update lockscreen background if you want (requires reload or extra hooks)
    # pkill swaylock; swaylock -f -i "$FULL_PATH" & # Example
fi
