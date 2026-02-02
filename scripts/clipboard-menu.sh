#!/usr/bin/env bash
# Clipboard menu using fuzzel + cliphist
cliphist list | fuzzel -d -p "Clipboard: " | cliphist decode | wl-copy
