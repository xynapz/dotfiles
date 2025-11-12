import os
import subprocess
from libqtile import hook

mod = "mod4"
terminal = "ghostty"

colors = {
    'bg': '#2e3440',
    'bg_alt': '#3b4252',
    'fg': '#d8dee9',
    'fg_alt': '#81a1c1',
    'accent': '#5e81ac',
    'red': '#bf616a',
    'orange': '#d08770',
    'yellow': '#ebcb8b',
    'green': '#a3be8c',
    'purple': '#b48ead',
    'cyan': '#88c0d0',
    'border': '#434c5e',
    'border_active': '#81a1c1',
}

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser("~")
    subprocess.call([home + "/.config/qtile/autostart.sh"])

@hook.subscribe.client_new
def assign_app_group(client):
    d = {}
    # Match by WM_CLASS
    d["2"] = ["emacs", "Emacs"]
    d["3"] = ["zen"]
    wm_class = client.window.get_wm_class()
    if wm_class:
        for group, classes in d.items():
            if any(wm_class_item in wm_class for wm_class_item in classes):
                client.togroup(group)
                break
