#!/usr/bin/env python3

import sys
import os
import subprocess
import signal
import ctypes
import stat

# Universal launcher wrapper
# Usage: 
#   List apps: universal-wrapper.py --scan
#   Run app:   universal-wrapper.py --run <command>

libc = ctypes.CDLL("libc.so.6")
PR_SET_PDEATHSIG = 1

def set_pdeathsig():
    # Set parent death signal to SIGTERM
    libc.prctl(PR_SET_PDEATHSIG, signal.SIGTERM)

def scan_path():
    paths = os.environ.get("PATH", "").split(os.pathsep)
    executables = set()
    
    for path in paths:
        if not path or not os.path.isdir(path):
            continue
            
        try:
            with os.scandir(path) as it:
                for entry in it:
                    if entry.is_file() and not entry.name.startswith("."):
                        # Check executability
                        try:
                            st = entry.stat()
                            if st.st_mode & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH):
                                executables.add(entry.name)
                        except OSError:
                            pass
        except OSError:
            pass
            
    # Print sorted unique executables
    for exe in sorted(executables):
        print(exe)

def run_command(raw_args):
    if not raw_args:
        sys.exit(0)

    # Some launchers might pass the command as a single string, others as list
    # If we get a listing pipeline, it might be separate.
    # But usually passed as: wrapper --run cmd arg1 arg2
    
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
        
        set_pdeathsig()
        
        final_args = []
        if real_cmd:
            final_args.append(real_cmd)
        final_args.extend(args)
        
        if not final_args:
            sys.exit(1)

        # Run attached with PDEATHSIG
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
    # If using bemenu pipeline, execute normally.
    try:
        os.execvp(cmd, raw_args)
    except FileNotFoundError:
        # Fallback if command not found (maybe typed manually?)
        print(f"Command not found: {cmd}", file=sys.stderr)
        sys.exit(127)

def main():
    if len(sys.argv) < 2:
        print("Usage: --scan OR --run <cmd>", file=sys.stderr)
        sys.exit(1)

    mode = sys.argv[1]
    
    if mode == "--scan":
        scan_path()
    elif mode == "--run":
        run_command(sys.argv[2:])
    else:
        # Assuming direct run if no flag? No, enforce flags for clarity in pipeline.
        print(f"Unknown mode: {mode}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
