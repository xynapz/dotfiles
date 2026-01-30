#!/usr/bin/env bash
# Clipboard menu using fuzzel + cliphist
cliphist list | fuzzel -d -p "Clipboard: " -w 60 -l 15 | cliphist decode | wl-copy
