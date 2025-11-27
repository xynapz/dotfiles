#!/usr/bin/env bash

# Game Mode Toggle Script for Rofi
# Toggles Hyprland animations and decorations for performance

LOCK_FILE="$HOME/.cache/gamemode.active"

if [ -z "$@" ]; then
    ICON="$HOME/dotfiles/.config/rofi/icons/game.png"
    if [ -f "$LOCK_FILE" ]; then
        echo -en "Disable Game Mode\0icon\x1f$ICON\n"
    else
        echo -en "Enable Game Mode\0icon\x1f$ICON\n"
    fi
else
    if [ "$@" = "Enable Game Mode" ]; then
        # Create lock file
        touch "$LOCK_FILE"
        
        # Disable animations and decorations
        hyprctl --batch "\
            keyword animations:enabled 0;\
            keyword decoration:shadow:enabled 0;\
            keyword decoration:blur:enabled 0;\
            keyword general:gaps_in 0;\
            keyword general:gaps_out 0;\
            keyword decoration:rounding 0" > /dev/null
            
        notify-send -u low -t 2000 "Game Mode" "Enabled: Animations & Effects Disabled"
        
    elif [ "$@" = "Disable Game Mode" ]; then
        # Remove lock file
        rm -f "$LOCK_FILE"
        
        # Reload Hyprland config to restore defaults
        hyprctl reload > /dev/null
        
        notify-send -u low -t 2000 "Game Mode" "Disabled: Normal Mode Restored"
    fi
fi
