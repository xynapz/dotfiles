# Mako Notification Daemon
{ config, pkgs, lib, ... }:

{
  services.mako = {
    enable = true;
    font = "Iosevka 12";
    backgroundColor = "#2e3440ee"; textColor = "#eceff4"; borderColor = "#4c566a";
    borderSize = 2; borderRadius = 8; width = 350; height = 150;
    margin = "10"; padding = "15"; anchor = "top-right";
    defaultTimeout = 5000; layer = "overlay"; maxVisible = 5;
    icons = true; maxIconSize = 48;
    extraConfig = ''
      [urgency=low]
      border-color=#a3be8c
      [urgency=critical]
      border-color=#bf616a
      default-timeout=0
    '';
  };
}
