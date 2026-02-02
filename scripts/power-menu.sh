#!/usr/bin/env bash
# Power menu using fuzzel
chosen=$(printf "Lock\nLogout\nSuspend\nReboot\nPower Off" | fuzzel -d -p "Power: ")
case "$chosen" in
    "Lock") swaylock -f ;;
    "Logout") swaymsg exit ;;
    "Suspend") systemctl suspend ;;
    "Reboot") systemctl reboot ;;
    "Power Off") systemctl poweroff ;;
esac
