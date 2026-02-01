# Fuzzel Launcher Configuration
{ config, pkgs, lib, ... }:

{
  programs.fuzzel = {
    enable = true;
  };
  xdg.configFile."fuzzel/fuzzel.ini".source = ../../config/fuzzel/fuzzel.ini;
}
