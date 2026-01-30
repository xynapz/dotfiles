# Sway Window Manager Configuration
{ config, pkgs, lib, ... }:

{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;

    config = {
      modifier = "Mod4";
      terminal = "wezterm";
      menu = "fuzzel";

      gaps = { inner = 6; outer = 0; smartGaps = true; smartBorders = "on"; };
      window = { border = 2; titlebar = false; };
      floating = { border = 2; titlebar = false; };

      input = {
        "type:touchpad" = { dwt = "enabled"; tap = "enabled"; natural_scroll = "enabled"; middle_emulation = "enabled"; };
        "type:keyboard" = { xkb_layout = "us"; xkb_options = "caps:escape"; };
      };

      output = {
        "HDMI-A-1" = { resolution = "2560x1440@99.9Hz"; position = "0,0"; };
        "eDP-1" = { resolution = "1920x1080"; position = "2560,360"; };
      };

      startup = [
        { command = "emacs --daemon"; }
        { command = "wl-paste --watch cliphist store"; }
        { command = "mako"; }
        { command = "waybar"; }
        { command = "swayidle -w timeout 600 'swaylock -f' timeout 1200 'swaymsg \"output * power off\"' resume 'swaymsg \"output * power on\"' before-sleep 'swaylock -f'"; }
        { always = true; command = "gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark' && gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'"; }
      ];

      keybindings = let mod = config.wayland.windowManager.sway.config.modifier; scripts = "$HOME/dotfiles/scripts"; in {
        "${mod}+Return" = "exec wezterm";
        "${mod}+Shift+Return" = "exec emacsclient -c";
        "${mod}+Shift+b" = "exec firefox";
        "${mod}+f" = "exec thunar";
        "${mod}+Shift+q" = "kill";
        "${mod}+Shift+c" = "reload";
        "${mod}+Shift+e" = "exec swaynag -t warning -m 'Exit Sway?' -B 'Yes' 'swaymsg exit'";

        "${mod}+space" = "exec fuzzel";
        "${mod}+v" = "exec ${scripts}/clipboard-menu.sh";
        "${mod}+Shift+p" = "exec ${scripts}/power-menu.sh";
        "${mod}+Mod1+l" = "exec swaylock -f";

        "XF86AudioMute" = "exec pamixer -t";
        "XF86AudioLowerVolume" = "exec pamixer -d 3";
        "XF86AudioRaiseVolume" = "exec pamixer -i 3";
        "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
        "XF86MonBrightnessUp" = "exec brightnessctl set +5%";

        "Print" = "exec grim -g \"$(slurp)\" - | swappy -f -";
        "Shift+Print" = "exec grim - | swappy -f -";
        "${mod}+Print" = "exec grim -g \"$(slurp)\" ~/Pictures/Screenshots/$(date +%Y%m%d-%H%M%S).png";
        "${mod}+p" = "exec ${scripts}/color-picker.sh";

        "${mod}+h" = "focus left"; "${mod}+j" = "focus down"; "${mod}+k" = "focus up"; "${mod}+l" = "focus right";
        "${mod}+Shift+h" = "move left"; "${mod}+Shift+j" = "move down"; "${mod}+Shift+k" = "move up"; "${mod}+Shift+l" = "move right";

        "${mod}+1" = "workspace number 1"; "${mod}+2" = "workspace number 2"; "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4"; "${mod}+5" = "workspace number 5"; "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7"; "${mod}+8" = "workspace number 8"; "${mod}+9" = "workspace number 9"; "${mod}+0" = "workspace number 10";

        "${mod}+Shift+1" = "move container to workspace number 1"; "${mod}+Shift+2" = "move container to workspace number 2";
        "${mod}+Shift+3" = "move container to workspace number 3"; "${mod}+Shift+4" = "move container to workspace number 4";
        "${mod}+Shift+5" = "move container to workspace number 5"; "${mod}+Shift+6" = "move container to workspace number 6";
        "${mod}+Shift+7" = "move container to workspace number 7"; "${mod}+Shift+8" = "move container to workspace number 8";
        "${mod}+Shift+9" = "move container to workspace number 9"; "${mod}+Shift+0" = "move container to workspace number 10";

        "${mod}+Control+h" = "move workspace to output left"; "${mod}+Control+l" = "move workspace to output right";
        "${mod}+b" = "splith"; "${mod}+s" = "layout stacking"; "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout toggle split"; "${mod}+Shift+f" = "fullscreen"; "${mod}+Shift+space" = "floating toggle";
        "${mod}+a" = "focus parent"; "${mod}+Shift+minus" = "move scratchpad"; "${mod}+minus" = "scratchpad show";
        "${mod}+r" = "mode resize";
      };

      modes.resize = {
        "h" = "resize shrink width 10px"; "j" = "resize grow height 10px";
        "k" = "resize shrink height 10px"; "l" = "resize grow width 10px";
        "Return" = "mode default"; "Escape" = "mode default";
      };
    };

    extraConfig = ''
      for_window [class=".*"] inhibit_idle fullscreen
      for_window [app_id=".*"] inhibit_idle fullscreen
      for_window [window_role="(pop-up|bubble|dialog)"] floating enable
      for_window [window_type="dialog"] floating enable
      for_window [app_id="(pavucontrol|nm-connection-editor|blueman-manager)"] floating enable
      for_window [app_id="(foot|emacs|thunar)"] border none
      for_window [app_id="^org\.(wezfurlong\.wezterm)$"] border none
      for_window [class="(firefox|brave)"] border none
      include /etc/sway/config.d/*
    '';
  };
}
