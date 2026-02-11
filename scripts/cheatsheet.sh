#!/usr/bin/env bash
# Cheatsheet launcher - shows fuzzel picker then opens EWW widget
# Triggered by Super+/ in sway

set -euo pipefail

SCRIPTS="$HOME/dotfiles/scripts"
EWW_CONFIG="$HOME/dotfiles/config/eww"

# Topic menu
TOPICS="  Sway\n  WezTerm\n  Bash\n  Emacs\n  Firefox"

# Show fuzzel picker
CHOICE=$(echo -e "$TOPICS" | fuzzel --dmenu --prompt "Cheatsheet: " --width 30 --lines 6)

# Strip icon prefix and whitespace
TOPIC=$(echo "$CHOICE" | sed 's/^[^ ]* //' | tr '[:upper:]' '[:lower:]' | xargs)

# Validate
case "$TOPIC" in
    sway|wezterm|bash|emacs|firefox) ;;
    *) exit 0 ;;
esac

# Parse data
DATA=$("$SCRIPTS/cheatsheet-parse.sh" "$TOPIC")

# Pretty title
case "$TOPIC" in
    sway)    TITLE="Sway Window Manager" ;;
    wezterm) TITLE="WezTerm Terminal" ;;
    bash)    TITLE="Bash Shell" ;;
    emacs)   TITLE="Doom Emacs" ;;
    firefox) TITLE="Firefox Browser" ;;
esac

# Close existing window if open
eww -c "$EWW_CONFIG" close cheatsheet 2>/dev/null || true

# Update variables and open
eww -c "$EWW_CONFIG" update cheatsheet_title="$TITLE"
eww -c "$EWW_CONFIG" update cheatsheet_data="$DATA"
eww -c "$EWW_CONFIG" open cheatsheet
