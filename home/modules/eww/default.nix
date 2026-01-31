{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ eww ];

  xdg.configFile."eww/eww.yuck".source = ./eww.yuck;
  xdg.configFile."eww/eww.scss".source = ./eww.scss;
}
