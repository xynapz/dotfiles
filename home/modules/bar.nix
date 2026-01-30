# Waybar Configuration
{ config, pkgs, lib, ... }:

{
  programs.waybar = {
    enable = true;
    settings.mainBar = {
      layer = "top"; position = "top"; height = 32; spacing = 4;
      modules-left = [ "sway/workspaces" "sway/mode" "sway/window" ];
      modules-center = [ "clock" ];
      modules-right = [ "tray" "cpu" "memory" "pulseaudio" "network" "bluetooth" "battery" ];

      "sway/workspaces" = { disable-scroll = false; format = "{name}"; };
      clock = { format = "{:%H:%M}"; format-alt = "{:%Y-%m-%d %H:%M:%S}"; interval = 1; };
      cpu = { format = " {usage}%"; interval = 2; };
      memory = { format = " {}%"; interval = 2; };
      battery = { format = "{icon} {capacity}%"; format-icons = [ "" "" "" "" "" ]; };
      network = { format-wifi = " {signalStrength}%"; format-disconnected = "⚠ Disconnected"; };
      bluetooth = { format = " {status}"; on-click = "blueman-manager"; };
      pulseaudio = { format = "{icon} {volume}%"; format-muted = " muted"; format-icons.default = [ "" "" "" ]; on-click = "pavucontrol"; };
      tray = { icon-size = 18; spacing = 8; };
    };

    style = ''
      * { font-family: "IBM Plex Mono", monospace; font-size: 14px; }
      window#waybar { background-color: rgba(30, 34, 42, 0.95); color: #eceff4; border-bottom: 2px solid #3b4252; }
      #workspaces button { padding: 0 8px; background-color: transparent; color: #d8dee9; }
      #workspaces button.focused { background-color: rgba(136, 192, 208, 0.3); border-bottom: 2px solid #88c0d0; }
      #clock { font-weight: bold; color: #88c0d0; }
      #battery { color: #a3be8c; } #cpu { color: #81a1c1; } #memory { color: #b48ead; }
      #network { color: #a3be8c; } #bluetooth { color: #5e81ac; } #pulseaudio { color: #ebcb8b; }
      #clock, #battery, #cpu, #memory, #network, #bluetooth, #pulseaudio, #tray { padding: 0 10px; }
    '';
  };
}
