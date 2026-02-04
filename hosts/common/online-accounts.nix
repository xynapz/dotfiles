# GNOME Online Accounts and Keyring Configuration
# Provides secure credential storage and OAuth integration with cloud services.
# After enabling, add accounts via: XDG_CURRENT_DESKTOP=GNOME gnome-control-center
{ config, pkgs, ... }:

{
  # Required for GNOME settings storage
  programs.dconf.enable = true;

  # GNOME Online Accounts (Google, Microsoft, Nextcloud, etc.)
  services.gnome.gnome-online-accounts.enable = true;

  # Calendar and Contacts sync backend
  services.gnome.evolution-data-server.enable = true;

  # Secure credential storage (encrypted keyring)
  services.gnome.gnome-keyring.enable = true;

  # GVFS for Google Drive mounting in Nautilus
  services.gvfs.enable = true;

  # Management tools
  environment.systemPackages = with pkgs; [
    gnome-control-center  # For adding online accounts
    seahorse              # GUI keyring manager
  ];
}
