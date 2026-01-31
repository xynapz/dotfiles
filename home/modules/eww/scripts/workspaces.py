#!/usr/bin/env python3

import subprocess
import json
import sys
import os
import socket

def get_workspaces():
    output = subprocess.check_output(["swaymsg", "-t", "get_workspaces", "-r"])
    return json.loads(output)

def listen():
    # Initial output
    print(json.dumps(get_workspaces()), flush=True)

    # Listen to sway events
    process = subprocess.Popen(
        ["swaymsg", "-t", "subscribe", "-m", '["workspace"]'],
        stdout=subprocess.PIPE,
        universal_newlines=True
    )

    for line in process.stdout:
        # On any workspace event, re-fetch the full list
        # This is simpler than maintaining state and robust enough for now
        print(json.dumps(get_workspaces()), flush=True)

if __name__ == "__main__":
    listen()
