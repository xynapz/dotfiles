#!/bin/bash

# Game Mode Script for Hyprland
# Toggles animations, blur, and other settings for performance vs aesthetics.

# Rofi menu options
OPTION_GAME="  Game Mode"
OPTION_DEV="  Dev Mode"

# Get choice
CHOICE=$(echo -e "$OPTION_GAME\n$OPTION_DEV" | rofi -dmenu -p "Select Mode" -theme-str 'window {width: 20%;} listview {lines: 2;}')

case "$CHOICE" in
    "$OPTION_GAME")
        # Game Mode: Disable animations, blur, shadows
        hyprctl keyword animations:enabled 0
        hyprctl keyword decoration:blur:enabled 0
        hyprctl keyword decoration:shadow:enabled 0
        hyprctl keyword misc:vfr 0 # Disable VFR for consistent frametimes in games (optional)
        
        # Set Env Vars (Note: Only affects new processes)
        hyprctl setenv WLR_NO_HARDWARE_CURSORS 1
        
        notify-send "Game Mode" "Performance settings enabled. Animations & Blur OFF."
        ;;
        
    "$OPTION_DEV")
        # Dev Mode: Enable animations, blur, shadows
        hyprctl keyword animations:enabled 1
        hyprctl keyword decoration:blur:enabled 1
        hyprctl keyword decoration:shadow:enabled 1
        hyprctl keyword misc:vfr 1
        
        # Reset Env Vars (Best effort)
        # hyprctl setenv WLR_NO_HARDWARE_CURSORS 0 
        
        notify-send "Dev Mode" "Aesthetics enabled. Animations & Blur ON."
        ;;
esac
