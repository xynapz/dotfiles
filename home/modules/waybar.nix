# Waybar Configuration for NixOS
{ config, pkgs, lib, ... }:

{
  programs.waybar = {
    enable = true;
  };

  xdg.configFile."waybar/config.jsonc".source = ../../config/waybar/config.jsonc;
  xdg.configFile."waybar/style.css".source = ../../config/waybar/style.css;
}
