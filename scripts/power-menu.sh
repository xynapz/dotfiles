#!/usr/bin/env bash
# Power menu using fuzzel
chosen=$(printf "  Lock\n  Logout\n  Suspend\n  Reboot\n  Power Off" | bemenu --prompt "Power: ")
case "$chosen" in
    *"Lock"*) swaylock -f ;;
    *"Logout"*) swaymsg exit ;;
    *"Suspend"*) systemctl suspend ;;
    *"Reboot"*) systemctl reboot ;;
    *"Power Off"*) systemctl poweroff ;;
esac
