#!/bin/sh

case $1 in
"vol")
    pamixer --get-volume
    ;;
"vol-icon")
    vol=$(pamixer --get-volume)
    if [ "$vol" -eq 0 ] || [ "$(pamixer --get-mute)" = "true" ]; then echo "🔇";
    elif [ "$vol" -lt 30 ]; then echo "🔈";
    elif [ "$vol" -lt 70 ]; then echo "🔉";
    else echo "🔊"; fi
    ;;
"mic")
    pamixer --default-source --get-volume
    ;;
"mic-status")
    if [ "$(pamixer --default-source --get-mute)" = "true" ]; then echo "mic-off"; else echo "mic-on"; fi
    ;;
"mic-toggle")
    pamixer --default-source --toggle-mute
    ;;
"speaker-set")
    pamixer --set-volume "$2"
    ;;
"mic-set")
    pamixer --default-source --set-volume "$2"
    ;;
esac
