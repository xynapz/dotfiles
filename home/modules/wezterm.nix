# WezTerm Configuration
{ config, pkgs, lib, ... }:

{
  xdg.configFile."wezterm/wezterm.lua".source = ../../config/wezterm/wezterm.lua;
}
