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
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  # GRAPHICS & WAYLAND
  hardware.graphics.enable = true;

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
    greetd
    bibata-cursors papirus-icon-theme # Themes for Greeter
    iosevka jetbrains-mono ibm-plex
    nerd-fonts.iosevka nerd-fonts.jetbrains-mono
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

  # GREETD (ReGreet - Graphical & Minimal)
  environment.etc."greetd/wallpaper.png".source = ../../wallpapers/abstract.png;
  environment.etc."greetd/sway-config".text = ''
    exec "${pkgs.regreet}/bin/regreet";
    include /etc/sway/config.d/*;
    output * bg /etc/greetd/wallpaper.png fill
    output HDMI-A-1 resolution 2560x1440@99.9Hz position 0,0
    output eDP-1 resolution 1920x1080 position 2560,360
  '';

  programs.regreet = {
    enable = true;
    settings = {
      background = {
        path = "/etc/greetd/wallpaper.png";
        fit = "Cover";
      };
      GTK = {
        application_prefer_dark_theme = true;
        cursor_theme_name = lib.mkForce "Bibata-Modern-Classic";
        icon_theme_name = lib.mkForce "Papirus-Dark";
        theme_name = lib.mkForce "Adwaita-dark";
      };
    };
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.sway}/bin/sway --config /etc/greetd/sway-config";
        user = "greeter";
      };
    };
  };

  # NIX SETTINGS
  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.gc = { automatic = true; dates = "weekly"; options = "--delete-older-than 14d"; };

  system.stateVersion = "24.11";
}
