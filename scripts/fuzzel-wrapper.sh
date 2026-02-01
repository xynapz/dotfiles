#!/usr/bin/env bash

# Fuzzel wrapper script
# Usage: Set `launch-prefix=~/dotfiles/scripts/fuzzel-wrapper.sh` in fuzzel.ini
#
# Syntax:
#   !cmd   -> Run 'cmd' in shell (blocking)
#   !!cmd  -> Run 'cmd' in background (detached/disowned)
#   cmd    -> Run 'cmd' normally (as application)

cmd="$1"

# Check if command starts with !! (run detached)
if [[ "$cmd" == "!!"* ]]; then
    # Remove the leading !!
    real_cmd="${cmd#!!}"
    # Run in background and disown
    nohup $real_cmd >/dev/null 2>&1 & disown
    exit 0
fi

# Check if command starts with ! (run command, possibly in terminal if needed, but handled by shell)
if [[ "$cmd" == "!"* ]]; then
    # Remove the leading !
    real_cmd="${cmd#!}"
    # Execute the command directly
    exec sh -c "$real_cmd"
fi

# Default: Execute the command as passed by fuzzel (normal application launch)
exec "$@"
