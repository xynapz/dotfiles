from libqtile.config import Key, Group
from libqtile.lazy import lazy
from core import mod, terminal

keys = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "space", lazy.layout.next()),

    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),

    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod], "n", lazy.layout.normalize()),

    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),
    Key([mod], "Return", lazy.spawn(terminal)),
    Key([mod], "d", lazy.spawn("rofi -show run -theme ~/.config/rofi/themes/dx-dark.rasi")),
    Key([mod], "e", lazy.spawn("emacsclient -c")),
    Key([mod], "b", lazy.spawn("zen-browser")),
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "c", lazy.window.kill()),
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "t", lazy.window.toggle_floating()),
    Key([mod, "control"], "r", lazy.reload_config()),
    Key([mod, "control"], "q", lazy.shutdown()),

    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%")),
    Key([], "XF86AudioMute", lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),

    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),

    Key([mod], "s", lazy.spawn("flameshot gui")),
    Key([mod, "shift"], "s", lazy.spawn("flameshot full")),
    Key([mod, "shift"], "p", lazy.spawn("/home/angeldx/dotfiles/scripts/dx-power.sh"), desc="Power menu"),
    Key([mod, "shift"], "v", lazy.spawn("/home/angeldx/dotfiles/scripts/dx-volalph.sh"), desc="Vol & Alpha Menu"),
]

group_definitions = [
    ("1", {"label": ""}),  # Terminal
    ("2", {"label": ""}),  # Code
    ("3", {"label": ""}),  # Browser
    ("4", {"label": ""}),  # Dev
    ("5", {"label": ""}),  # Files
    ("6", {"label": ""}),  # Music
    ("7", {"label": ""}),  # Images
    ("8", {"label": ""}),  # Chat
    ("9", {"label": ""}),  # Settings
]

groups = [Group(name, **kwargs) for name, kwargs in group_definitions]

for name, _ in group_definitions:
    keys.extend([
        Key([mod], name, lazy.group[name].toscreen(), desc=f"Switch to group {name}"),
        Key([mod, "shift"], name, lazy.window.togroup(name, switch_group=True), desc=f"Move focused window to group {name}"),
        Key([mod, "mod1"], name, lazy.window.togroup(name), desc=f"Move window to group {name}"),
    ])
