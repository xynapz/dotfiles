# Fuzzel Launcher Configuration
{ config, pkgs, lib, ... }:

{
  programs.fuzzel = {
    enable = true;
    settings = {
      main = { font = "IBM Plex Mono:size=14"; width = 50; lines = 12; prompt = "❯ "; terminal = "wezterm"; icons-enabled = "yes"; };
      colors = { background = "2e3440ee"; text = "eceff4ff"; prompt = "88c0d0ff"; match = "88c0d0ff"; selection = "3b4252ff"; selection-text = "eceff4ff"; border = "4c566aff"; };
      border = { width = 2; radius = 8; };
    };
  };
}
