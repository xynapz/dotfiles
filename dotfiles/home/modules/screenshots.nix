# Screenshot Configuration (Swappy)
{ config, pkgs, lib, ... }:

{
  xdg.configFile."swappy/config".source = ../../.config/swappy/config;
  home.activation.createScreenshotDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/Pictures/Screenshots"
  '';
}
