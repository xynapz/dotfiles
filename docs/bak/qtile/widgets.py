from libqtile import bar, widget
from libqtile.config import Screen
from core import colors

widget_defaults = dict(
    font='JetBrainsMono Nerd Font',
    fontsize=14,
    padding=8,
    background=colors['bg'],
    foreground=colors['fg'],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar([
            # Workspaces
            widget.GroupBox(
                fontsize=13,
                highlight_method='line',
                active=colors['fg'],
                inactive=colors['fg_alt'],
                this_current_screen_border=colors['accent'],
                this_screen_border=colors['fg_alt'],
                other_current_screen_border=colors['fg_alt'],
                other_screen_border=colors['fg_alt'],
                margin_x=4,
                padding_x=6,
                disable_drag=True,
                use_mouse_wheel=False,
                rounded=False,
            ),

            # Window Title
            widget.Spacer(length=8),
            widget.WindowName(
                format='{name}',
                max_chars=50,
                padding=4,
            ),

            # Spacer to right-align system info
            widget.Spacer(),

            # Memory
            widget.Memory(
                format=' {MemUsed:.0f}MB',
                foreground=colors['green'],
                padding=6,
            ),

            widget.Volume(
                fmt=' {}%',
                padding=6,
            ),

            # Battery
            widget.Battery(
                format=' {percent:2.0%}',
                padding=6,
            ),

            # Clock
            widget.Clock(
                format=' %a %b %d   %H:%M:%S',
                padding=6,
            ),

            # Systray (optional, on primary screen only)
            widget.Systray(padding=4, icon_size=16),

            # Layout Icon
            widget.CurrentLayoutIcon(
                scale=0.7,
                padding=4,
            ),
        ], 24, background=colors['bg'], opacity=1.0)
    )
]
