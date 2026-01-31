# Eww Bar Configuration for Sway
{ config, pkgs, lib, ... }:

{
  programs.eww = {
    enable = true;
    package = pkgs.eww;
    configDir = ./eww;
  };

  home.packages = with pkgs; [
    eww
    socat
    networkmanager
    procps
  ];
}
