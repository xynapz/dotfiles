# Waydroid - Android container for running Android apps
# Requires Wayland compositor (Sway) and one-time initialization:
#   sudo waydroid init -s GAPPS -f
{ config, pkgs, lib, ... }:

{
  # Enable Waydroid container
  virtualisation.waydroid.enable = true;
}
