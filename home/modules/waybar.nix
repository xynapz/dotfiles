# Waybar Configuration for NixOS
{ config, pkgs, lib, ... }:

{
  programs.waybar = {
    enable = true;
    systemd.enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 38;
        spacing = 0;

        modules-left = [
          "custom/brand"
          "sway/workspaces"
          "custom/window-icon"
          "sway/window"
        ];

        modules-center = [
          "clock"
        ];

        modules-right = [
          "group/hardware"
          "pulseaudio"
          "backlight"
          "battery"
          "tray"
          "custom/notification"
          "custom/power"
        ];

        # Module Configurations
        "custom/brand" = {
          format = " ";
          tooltip = false;
        };

        "sway/workspaces" = {
          disable-scroll = false;
          all-outputs = false;
          format = "{icon}";
          format-icons = {
            "1" = "1";
            "2" = "2";
            "3" = "3";
            "4" = "4";
            "5" = "5";
            "6" = "6";
            "7" = "7";
            "8" = "8";
            "9" = "9";
            "10" = "10";
            urgent = "";
            focused = "";
            default = "";
          };
        };

        "custom/window-icon" = {
          format = "";
          tooltip = false;
        };

        "sway/window" = {
          max-length = 50;
          tooltip = false;
        };

        clock = {
          interval = 1;
          format = "{:%H:%M:%S}";
          format-alt = "{:%A, %B %d, %Y}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "month";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            on-click-right = "mode";
            format = {
              months = "<span color='#3daee9'><b>{}</b></span>";
              days = "<span color='#eff0f1'><b>{}</b></span>";
              weeks = "<span color='#1cdc9a'><b>W{}</b></span>";
              weekdays = "<span color='#f67400'><b>{}</b></span>";
              today = "<span color='#da4453'><b><u>{}</u></b></span>";
            };
          };
        };

        "group/hardware" = {
          orientation = "horizontal";
          modules = [
            "network"
            "cpu"
            "memory"
            "disk"
          ];
        };

        network = {
          interval = 3;
          format-wifi = "  {signalStrength}%";
          format-ethernet = " ";
          format-disconnected = "󰖪 ";
          tooltip-format = "{ifname}: {ipaddr}/{cidr}";
          tooltip-format-wifi = "{essid} ({signalStrength}%): {ipaddr}";
          tooltip-format-ethernet = "{ifname}: {ipaddr}/{cidr}";
          tooltip-format-disconnected = "Disconnected";
          on-click = "nm-connection-editor";
        };

        cpu = {
          interval = 3;
          format = "  {usage}%";
          tooltip = true;
        };

        memory = {
          interval = 3;
          format = "  {percentage}%";
          tooltip-format = "{used:0.1f}G / {total:0.1f}G used";
        };

        disk = {
          interval = 30;
          format = "  {percentage_used}%";
          path = "/";
          tooltip-format = "{used} / {total} used ({percentage_used}%)";
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-muted = "  0%";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" "" ];
          };
          on-click = "pavucontrol";
          tooltip-format = "{desc}: {volume}%";
        };

        backlight = {
          device = "intel_backlight";
          format = "{icon} {percent}%";
          format-icons = [ "" "" "" "" "" "" "" "" "" ];
          on-scroll-up = "brightnessctl set +5%";
          on-scroll-down = "brightnessctl set 5%-";
        };

        battery = {
          interval = 10;
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          format-icons = [ "" "" "" "" "" ];
          tooltip-format = "{timeTo}, {capacity}%";
        };

        tray = {
          icon-size = 16;
          spacing = 8;
        };

        "custom/notification" = {
          tooltip = false;
          format = "{icon}";
          format-icons = {
            notification = "<span foreground='red'><sup></sup></span>";
            none = "";
            dnd-notification = "<span foreground='red'><sup></sup></span>";
            dnd-none = "";
            inhibited-notification = "<span foreground='red'><sup></sup></span>";
            inhibited-none = "";
            dnd-inhibited-notification = "<span foreground='red'><sup></sup></span>";
            dnd-inhibited-none = "";
          };
          return-type = "json";
          exec-if = "which swaync-client";
          exec = "swaync-client -swb";
          on-click = "swaync-client -t -sw";
          on-click-right = "swaync-client -d -sw";
          escape = true;
        };

        "custom/power" = {
          format = "⏻";
          tooltip = false;
          on-click = "$HOME/dotfiles/scripts/power-menu.sh";
        };
      };
    };

    style = ''
      /* Waybar Styles - Polished Pill Design v2 */

      * {
          border: none;
          border-radius: 0;
          font-family: "JetBrainsMono Nerd Font", "Noto Sans", sans-serif;
          font-size: 14px;
          font-weight: bold;
          min-height: 0;
      }

      window#waybar {
          background: rgba(43, 43, 43, 0.9);
          color: #eff0f1;
      }

      tooltip {
          background: #2b2b2b;
          border: 1px solid #3daee9;
          border-radius: 10px;
      }

      tooltip label {
          color: #eff0f1;
      }

      /* -----------------------------------------------------------------------------
       * Module Styles
       * -------------------------------------------------------------------------- */

      /* Standard Pill Style */
      #clock,
      #battery,
      #cpu,
      #memory,
      #network,
      #pulseaudio,
      #tray,
      #backlight,
      #custom-notification,
      #custom-power,
      #language,
      #disk {
          padding: 0 15px;
          margin: 4px 3px;
          background-color: #31363b;
          color: #eff0f1;
          border-radius: 15px;
          transition: all 0.3s ease;
      }

      /* Hover Effects */
      #clock:hover,
      #battery:hover,
      #cpu:hover,
      #memory:hover,
      #network:hover,
      #pulseaudio:hover,
      #tray:hover,
      #backlight:hover,
      #custom-notification:hover,
      #custom-power:hover {
          background-color: #3daee9;
          color: #2b2b2b;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
      }

      #pulseaudio {
          color: #f67400;
      }

      #backlight {
          color: #fdcc31;
      }

      /* -----------------------------------------------------------------------------
       * Brand Icon
       * -------------------------------------------------------------------------- */

      #custom-brand {
          background-color: #3daee9;
          color: #2b2b2b;
          font-family: "Iosevka", "JetBrainsMono Nerd Font", sans-serif;
          font-size: 16px;
          font-weight: 900;
          padding: 0px 12px 0px 8px;
          margin: 7px 7px;
          border-radius: 10px;
          transition: all 0.3s ease;
      }

      #custom-brand:hover {
          background-color: #eff0f1;
          color: #3daee9;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
      }

      /* -----------------------------------------------------------------------------
       * Workspace Styles
       * -------------------------------------------------------------------------- */

      #workspaces {
          background-color: #31363b;
          margin: 4px 15px 4px 3px;
          padding: 0 5px;
          border-radius: 15px;
      }

      #workspaces button {
          padding: 0 8px;
          margin: 4px 2px;
          background-color: transparent;
          color: #bdc3c7;
          border-radius: 10px;
          transition: all 0.3s ease;
      }

      #workspaces button:hover {
          background-color: #3daee9;
          color: #2b2b2b;
      }

      #workspaces button.active {
          background-color: #3daee9;
          color: #2b2b2b;
          min-width: 35px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
      }

      #workspaces button.urgent {
          background-color: #da4453;
          color: #eff0f1;
      }

      /* -----------------------------------------------------------------------------
       * Specific Module Tweaks
       * -------------------------------------------------------------------------- */

      #custom-window-icon {
          color: #eff0f1;
          padding-right: 5px;
          margin-left: 5px;
      }

      #window {
          background-color: transparent;
          color: #eff0f1;
          margin: 0 5px;
          padding: 0 5px;
      }

      #group-hardware {
          background-color: #31363b;
          padding: 0 5px 0 2px;
          margin: 4px 3px;
          border-radius: 15px;
      }

      #group-hardware #network {
          background-color: transparent;
          margin: 0;
          padding: 0 5px 0 2px;
          border-radius: 0;
          box-shadow: none;
      }

      #group-hardware #cpu {
          background-color: transparent;
          margin: 0;
          padding: 0 5px;
          border-radius: 0;
          box-shadow: none;
      }

      #group-hardware #memory {
          background-color: transparent;
          margin: 0;
          padding: 0px 5px 0px 2px;
          border-radius: 0;
          box-shadow: none;
      }

      #group-hardware #disk {
          background-color: transparent;
          margin: 0;
          padding: 0 5px;
          border-radius: 0;
          box-shadow: none;
      }

      /* Colors */
      #network {
          color: #3daee9;
      }

      #cpu {
          color: #da4453;
      }

      #memory {
          color: #1cdc9a;
      }

      #custom-power {
          background-color: #da4453;
          color: #eff0f1;
          padding: 0 12px;
      }

      #pulseaudio,
      #battery {
          padding: 0 16px 0 8px;
      }

      #backlight,
      #custom-power {
          padding: 0 12px 0 8px;
      }

      #custom-power:hover {
          background-color: #eff0f1;
          color: #da4453;
      }

      #battery.charging {
          color: #1cdc9a;
      }

      #battery.warning:not(.charging) {
          color: #f67400;
      }

      #battery.critical:not(.charging) {
          color: #da4453;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      @keyframes blink {
          to {
              background-color: #da4453;
              color: #eff0f1;
          }
      }
    '';
  };
}
