from libqtile import layout
from libqtile.config import Match
from core import colors

layouts = [
    layout.Columns(border_focus=colors['border_active'], border_normal=colors['border'], border_width=1, margin=2),
    layout.Max(),
    layout.MonadTall(border_focus=colors['border_active'], border_normal=colors['border'], border_width=1, margin=2),
    layout.MonadWide(border_focus=colors['border_active'], border_normal=colors['border'], border_width=1, margin=2),
    layout.Stack(border_focus=colors['border_active'], border_normal=colors['border'], border_width=1, margin=2, num_stacks=2),
]

floating_layout = layout.Floating(
    border_focus=colors['border_active'],
    border_normal=colors['border'],
    border_width=1,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
        Match(wm_class="Pavucontrol"),
        Match(wm_class="Blueman-manager"),
        Match(wm_class="Nm-connection-editor"),
        Match(wm_class="Rofi"),
        Match(wm_class="flameshot"),
    ]
)
