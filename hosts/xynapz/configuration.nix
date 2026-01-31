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

  # DISPLAY MANAGER (SDDM)
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    theme = "sugar-candy";
    package = pkgs.kdePackages.sddm;
  };

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

  # SYSTEM PACKAGES
  environment.systemPackages = with pkgs; [
    git vim wget curl htop unzip ripgrep fd jq tree
    pciutils usbutils lshw
    bibata-cursors papirus-icon-theme # Themes for Greeter
    iosevka jetbrains-mono ibm-plex
    nerd-fonts.iosevka nerd-fonts.jetbrains-mono

    # SDDM Sugar Candy Theme
    (pkgs.libsForQt5.callPackage "${pkgs.fetchFromGitHub {
      owner = "Kangie";
      repo = "sddm-sugar-candy";
      rev = "a1bfb85babde7daf4119083d8c4ef04a2f2cab3a";
      sha256 = "sha256-p4oWMx5q2+MSTQiYZdU1Fy5BhKdlpqBBqLzfzrqt+rU=";
    }}" {})
  ];

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
  nix.gc = { automatic = true; dates = "weekly"; options = "--delete-older-than 14d"; };

  system.stateVersion = "25.11";
}
