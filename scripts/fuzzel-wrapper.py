#!/usr/bin/env python3

import sys
import os
import subprocess
import signal
import ctypes

# Fuzzel wrapper script (Python version)
# Usage: Set `launch-prefix=~/dotfiles/scripts/fuzzel-wrapper.py` in fuzzel.ini
#
# Syntax:
#   !cmd   -> Run 'cmd' in shell (Attached/Dependent). Dies if wrapper/fuzzel dies.
#   !!cmd  -> Run 'cmd' in background (Detached). Persists.
#   cmd    -> Run 'cmd' normally (as application).

libc = ctypes.CDLL("libc.so.6")
PR_SET_PDEATHSIG = 1

def set_pdeathsig():
    # Set parent death signal to SIGTERM
    libc.prctl(PR_SET_PDEATHSIG, signal.SIGTERM)

def main():
    if len(sys.argv) < 2:
        sys.exit(0)

    # sys.argv[0] is the script, sys.argv[1] is the command fuzzel passes
    # Note: Fuzzel passes the command + args as separate arguments to the prefix script
    # e.g. prefix script.py command arg1 arg2
    
    raw_args = sys.argv[1:]
    if not raw_args:
        sys.exit(0)

    cmd = raw_args[0]
    args = raw_args[1:]

    # Handle !! (Detached)
    if cmd.startswith("!!"):
        real_cmd = cmd[2:]
        final_args = []
        if real_cmd:
            final_args.append(real_cmd)
        final_args.extend(args)
        
        if not final_args:
            sys.exit(1)

        # Run detached
        subprocess.Popen(
            final_args,
            start_new_session=True, # Detach from terminal/parent session
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        sys.exit(0)

    # Handle ! (Attached / Die with Parent)
    if cmd.startswith("!"):
        real_cmd = cmd[1:]
        # Join args back into a shell string if we are using shell=True, 
        # BUT the user usually types "!pkill waybar" which comes as ["!pkill", "waybar"]
        # So we probably want to run the command directly but bind its lifetime.
        
        final_args = []
        if real_cmd:
            final_args.append(real_cmd)
        final_args.extend(args)
        
        if not final_args:
            sys.exit(1)

        # Run attached with PDEATHSIG
        # We need to wait for it, because if we exit, it dies (if we set pdeathsig correctly).
        # Actually, if WE exit, Fuzzel exits.
        # So we just run it and block.
        p = subprocess.Popen(
            final_args,
            preexec_fn=set_pdeathsig
        )
        p.wait()
        sys.exit(p.returncode)

    # Normal Execution
    # Replace current process
    os.execvp(cmd, raw_args)

if __name__ == "__main__":
    main()
