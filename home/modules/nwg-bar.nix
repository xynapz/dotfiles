{ pkgs, ... }:

{
  home.packages = [ pkgs.nwg-bar ];

  xdg.configFile."nwg-bar/bar.json".text = ''
    [
      {
        "label": "Lock",
        "exec": "swaylock -f -c 000000",
        "icon": "system-lock-screen"
      },
      {
        "label": "Logout",
        "exec": "swaymsg exit",
        "icon": "system-log-out"
      },
      {
        "label": "Reboot",
        "exec": "systemctl reboot",
        "icon": "system-reboot"
      },
      {
        "label": "Shutdown",
        "exec": "systemctl -i poweroff",
        "icon": "system-shutdown"
      }
    ]
  '';

  xdg.configFile."nwg-bar/style.css".text = ''
    window {
        background-color: rgba(43, 43, 43, 0.9);
        color: #eff0f1;
    }

    #outer-box {
        margin: 0px;
    }

    #inner-box {
        background-color: #2b2b2b;
        border-radius: 15px;
        border: 1px solid #3daee9;
        padding: 20px;
        margin: 20px;
    }

    button, image {
        background: none;
        border: none;
        box-shadow: none;
    }

    button {
        background-color: #31363b;
        color: #eff0f1;
        border-radius: 15px;
        padding: 10px 20px;
        margin: 5px;
        transition: all 0.3s ease;
    }

    button:hover {
        background-color: #3daee9;
        color: #2b2b2b;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
    }
  '';
}
