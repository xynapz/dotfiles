#!/usr/bin/env bash
# Power menu using fuzzel
# Replaces DMS power menu

set -euo pipefail

chosen=$(printf "  Lock\n  Logout\n  Suspend\n  Reboot\n  Power Off" | fuzzel -d -p "Power: " -w 25 -l 5)

case "$chosen" in
    *"Lock"*)
        swaylock -f
        ;;
    *"Logout"*)
        swaymsg exit
        ;;
    *"Suspend"*)
        systemctl suspend
        ;;
    *"Reboot"*)
        systemctl reboot
        ;;
    *"Power Off"*)
        systemctl poweroff
        ;;
esac
