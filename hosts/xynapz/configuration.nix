# NixOS System Configuration for xynapz
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # BOOT
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # NETWORKING
  networking.hostName = "xynapz";
  networking.networkmanager.enable = true;

  # LOCALE & TIME
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_US.UTF-8";

  # GRAPHICS & WAYLAND
  hardware.graphics.enable = true;

  # DISPLAY MANAGER (SDDM) - Multi-monitor setup
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    package = pkgs.kdePackages.sddm;
    extraPackages = with pkgs; [
      kdePackages.qtmultimedia
      kdePackages.qtsvg
      kdePackages.qtvirtualkeyboard
    ];
    theme = "sddm-astronaut-theme";
    settings = {
      General = {
        DisplayServer = "wayland";
        GreeterEnvironment = "QT_SCREEN_SCALE_FACTORS=1;QT_WAYLAND_SHELL_INTEGRATION=layer-shell";
      };
      Wayland = {
        CompositorCommand = "${pkgs.kdePackages.kwin}/bin/kwin_wayland --no-lockscreen --no-global-shortcuts --locale1";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    git vim wget curl htop unzip ripgrep fd jq tree
    pciutils usbutils lshw
    bibata-cursors papirus-icon-theme
    iosevka jetbrains-mono ibm-plex
    nerd-fonts.iosevka nerd-fonts.jetbrains-mono
    sddm-astronaut
  ];



  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      swaylock swayidle wl-clipboard wl-clip-persist cliphist
      grim slurp mako fuzzel waybar brightnessctl pamixer playerctl
    ];
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # AUDIO (PipeWire)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # BLUETOOTH
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # USER
  users.users.xynapz = {
    isNormalUser = true;
    description = "xynapz";
    extraGroups = [ "wheel" "networkmanager" "video" "audio" "docker" ];
    shell = pkgs.bash;
  };

  # FONTS
  fonts.packages = with pkgs; [
    iosevka jetbrains-mono ibm-plex noto-fonts noto-fonts-color-emoji
    nerd-fonts.iosevka nerd-fonts.jetbrains-mono
  ];
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [ "IBM Plex Mono" ];
      sansSerif = [ "IBM Plex Sans" "Noto Sans" ];
    };
  };

  # SERVICES
  virtualisation.docker.enable = true;
  security.polkit.enable = true;
  services.dbus.enable = true;

  # NIX SETTINGS
  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.gc = { automatic = true; dates = "weekly"; options = "--delete-older-than 7d"; };

  system.stateVersion = "25.11";
}
