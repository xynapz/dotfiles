# Mako Notification Daemon
{ config, pkgs, lib, ... }:

{
  services.mako = {
    enable = true;
    layer = "overlay"; maxVisible = 5;
    iconPath = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark";
    icons = true; maxIconSize = 48;
    defaultTimeout = 5000;
    settings = {
      "default" = {
        font = "IBM Plex Mono 12";
        background-color = "#2e3440ee"; text-color = "#eceff4"; border-color = "#4c566a";
        border-size = 2; border-radius = 8; width = 350; height = 150;
        margin = "10"; padding = "15"; anchor = "top-right";
      };
      "urgency=low" = { border-color = "#a3be8c"; };
      "urgency=critical" = { border-color = "#bf616a"; default-timeout = 0; };
    };
  };
}
