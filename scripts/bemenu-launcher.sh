#!/bin/sh

# Source the theme
. "$(dirname "$0")/bemenu-theme.sh"

# Launch the menu pipeline
# 1. Scan apps with wrapper
# 2. Run bemenu (uses BEMENU_OPTS from theme)
# 3. Run selected command with wrapper
"$(dirname "$0")/universal-wrapper.py" --scan | \
    bemenu | \
    xargs -r "$(dirname "$0")/universal-wrapper.py" --run
