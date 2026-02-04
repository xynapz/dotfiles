# SDDM Configuration for NixOS
{ config, pkgs, ... }:

let
  custom-sddm-astronaut = pkgs.sddm-astronaut.override {
    embeddedTheme = "hyprland_kath";
  };

in {
  services.displayManager.sddm = {
    enable = true;
    wayland = {
      enable = true;
    };
    package = pkgs.kdePackages.sddm;
    autoNumlock = true;
    enableHidpi = true;
    theme = "sddm-astronaut-theme";
    settings = {
      General = {
        DisplayServer = "wayland";
        GreeterEnvironment = "QT_SCREEN_SCALE_FACTORS=1;QT_WAYLAND_SHELL_INTEGRATION=layer-shell";
      };
      Theme = {
        Current = "sddm-astronaut-theme";
        CursorTheme = "Bibata-Modern-Classic";
        CursorSize = 24;
      };
      Wayland = {
        CompositorCommand = "${pkgs.kdePackages.kwin}/bin/kwin_wayland --no-lockscreen --no-global-shortcuts --locale1";
      };
    };
    extraPackages = with pkgs; [
      custom-sddm-astronaut
      kdePackages.qtmultimedia
      kdePackages.qtsvg
      kdePackages.qtvirtualkeyboard
    ];
  };

  environment.systemPackages = with pkgs; [
    custom-sddm-astronaut
  ];

  # Auto-unlock gnome-keyring on login
  security.pam.services.sddm.enableGnomeKeyring = true;
}