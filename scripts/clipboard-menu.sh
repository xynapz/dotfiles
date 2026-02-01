#!/usr/bin/env bash
# Clipboard menu using fuzzel + cliphist
cliphist list | bemenu --prompt "Clipboard: " | cliphist decode | wl-copy
