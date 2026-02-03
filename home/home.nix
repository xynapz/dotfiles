# Home Manager Configuration for xynapz
{ config, pkgs, lib, ... }:

{
  imports = [
    ./modules/shell.nix
    ./modules/wezterm.nix
    ./modules/sway.nix
    ./modules/waybar.nix
    ./modules/notifications.nix
    ./modules/screenshots.nix
    ./modules/lockscreen.nix
    ./modules/emacs.nix
    ./modules/git.nix
    ./modules/fuzzel.nix
    ./modules/theme.nix
  ];

  home.username = "xynapz";
  home.homeDirectory = "/home/xynapz";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;

  # SESSION VARIABLES
  home.sessionVariables = {
    TERMINAL = "wezterm";
    EDITOR = "emacs -nw";
    VISUAL = "emacs -nw";
    PAGER = "less";
    LESS = "-R -i -M -S -x4";
    XDG_CURRENT_DESKTOP = "sway";
    XDG_SESSION_TYPE = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };

  # SESSION PATH
  home.sessionPath = [
    "$HOME/.emacs.d/bin"
    "$HOME/go/bin"
    "$HOME/.local/bin"
    "$HOME/bin"
    "$HOME/dotfiles/scripts"
  ];

  home.packages = with pkgs; [
    home-manager
    wezterm firefox texliveFull poppler-utils
    nodejs python3 go rustup gcc gnumake cmake
    ripgrep fd bat eza dust duf procs bottom fastfetch
    imv mpv wl-clipboard cliphist grim slurp swappy wf-recorder
    pavucontrol libnotify unzip p7zip tree calc
    iosevka jetbrains-mono sourceHighlight imagemagick
    xarchiver nixd antigravity discord
    kdePackages.kate kdePackages.okular kdePackages.kcolorchooser
    kdePackages.kcalc kdePackages.elisa kdePackages.gwenview
    gnome-calculator gnome-calendar gnome-pomodoro nautilus
  ];

  # XDG
  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
}
