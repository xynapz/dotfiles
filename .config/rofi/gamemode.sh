#!/usr/bin/env bash

# Game Mode Toggle Script for Rofi
# Toggles Hyprland animations and decorations for performance

LOCK_FILE="$HOME/.cache/gamemode.active"

if [ -z "$@" ]; then
    if [ -f "$LOCK_FILE" ]; then
        echo "Disable Game Mode"
    else
        echo "Enable Game Mode"
    fi
else
    if [ "$@" = "Enable Game Mode" ]; then
        # Create lock file
        touch "$LOCK_FILE"
        
        # Disable animations and decorations
        hyprctl --batch "\
            keyword animations:enabled 0;\
            keyword decoration:drop_shadow 0;\
            keyword decoration:blur:enabled 0;\
            keyword general:gaps_in 0;\
            keyword general:gaps_out 0;\
            keyword decoration:rounding 0"
            
        notify-send -u low -t 2000 "Game Mode" "Enabled: Animations & Effects Disabled"
        
    elif [ "$@" = "Disable Game Mode" ]; then
        # Remove lock file
        rm -f "$LOCK_FILE"
        
        # Reload Hyprland config to restore defaults
        hyprctl reload
        
        notify-send -u low -t 2000 "Game Mode" "Disabled: Normal Mode Restored"
    fi
fi
