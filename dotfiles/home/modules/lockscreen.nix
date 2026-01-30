# Swaylock Configuration
{ config, pkgs, lib, ... }:

{
  programs.swaylock = {
    enable = true;
    settings = {
      color = "2e3440";
      inside-color = "2e3440"; inside-ver-color = "5e81ac"; inside-wrong-color = "bf616a";
      ring-color = "4c566a"; ring-ver-color = "5e81ac"; ring-wrong-color = "bf616a";
      key-hl-color = "88c0d0"; bs-hl-color = "bf616a";
      text-color = "eceff4"; text-ver-color = "eceff4"; text-wrong-color = "eceff4";
      font = "Iosevka"; font-size = 24;
      indicator-radius = 100; indicator-thickness = 10;
      ignore-empty-password = true; show-failed-attempts = true;
    };
  };
}
