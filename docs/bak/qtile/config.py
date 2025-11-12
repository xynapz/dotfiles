from libqtile import hook
from libqtile.config import Screen
from libqtile.lazy import lazy

from core import colors, start_once
from keys import keys, groups, mod
from layouts import layouts, floating_layout
from widgets import screens, widget_defaults, extension_defaults
from mouse import mouse

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True

wmname = "LG3D"
