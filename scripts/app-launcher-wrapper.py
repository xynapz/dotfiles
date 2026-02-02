#!/usr/bin/env python3

import sys
import os
import subprocess
import signal
import ctypes

# App Launcher Wrapper for Fuzzel
# Handles:
#   !!cmd -> Detached execution (starts in new session)
#   !cmd  -> Attached execution (dies with parent/fuzzel, essentially)
#   cmd   -> Normal execution (replaces process)

libc = ctypes.CDLL("libc.so.6")
PR_SET_PDEATHSIG = 1

def set_pdeathsig():
    # Set parent death signal to SIGTERM
    libc.prctl(PR_SET_PDEATHSIG, signal.SIGTERM)

def main():
    if len(sys.argv) < 2:
        sys.exit(0)

    # Fuzzel passes the command as arguments.
    # e.g. wrapper.py firefox --private-window
    
    # We need to reconstruct the command or just parse the first arg depending on how we handle prefixes
    # Usually users type "!cmd" as one token in Fuzzel if it's a custom command, 
    # but if they pick a desktop entry, it might be complex.
    # However, '!!' is usually manually typed.
    
    # Let's inspect the first argument check for prefixes.
    # If the user typed "!!term -e htop", argv might be ["wrapper", "!!term", "-e", "htop"]
    # or ["wrapper", "!!term -e htop"] depending on how fuzzel tokenizes. 
    # Fuzzel usually tokenizes.
    
    cmd = sys.argv[1]
    args = sys.argv[2:]

    # Handle !! (Detached)
    if cmd.startswith("!!"):
        real_cmd_name = cmd[2:]
        if not real_cmd_name and args:
            real_cmd_name = args.pop(0)
        
        if not real_cmd_name:
            sys.exit(1)
            
        final_args = [real_cmd_name] + args
        
        # Run detached
        subprocess.Popen(
            final_args,
            start_new_session=True,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        sys.exit(0)

    # Handle ! (Attached / Die with Parent)
    if cmd.startswith("!"):
        real_cmd_name = cmd[1:]
        if not real_cmd_name and args:
            real_cmd_name = args.pop(0)

        if not real_cmd_name:
            sys.exit(1)

        final_args = [real_cmd_name] + args

        # Run attached with PDEATHSIG
        # We need to wait for it so we don't exit immediately (which would make Fuzzel exit, closing the pipe/parent)
        # Actually Fuzzel waits for the launch-prefix process?
        # If Fuzzel spawns wrapper, wrapper spawns app.
        # If wrapper replaces itself with app, Fuzzel waits for app.
        # If wrapper spawns app and waits, Fuzzel waits for wrapper.
        
        p = subprocess.Popen(
            final_args,
            preexec_fn=set_pdeathsig
        )
        try:
            p.wait()
        except KeyboardInterrupt:
            p.send_signal(signal.SIGTERM)
            p.wait()
        
        sys.exit(p.returncode)

    # Normal Execution
    # Replace current process
    # If the user selected an entry "Firefox", cmd is "firefox"
    try:
        os.execvp(cmd, sys.argv[1:])
    except FileNotFoundError:
        # Fallback
        print(f"Command not found: {cmd}", file=sys.stderr)
        sys.exit(127)

if __name__ == "__main__":
    main()
