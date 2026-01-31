# Home Manager Configuration for xynapz
{ config, pkgs, lib, ... }:

{
  imports = [
    ./modules/shell.nix
    ./modules/wezterm.nix
    ./modules/sway.nix
    ./modules/bar.nix
    ./modules/launcher.nix
    ./modules/notifications.nix
    ./modules/screenshots.nix
    ./modules/lockscreen.nix
    ./modules/doom-emacs.nix
    ./modules/git.nix
  ];

  home.username = "xynapz";
  home.homeDirectory = "/home/xynapz";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;

  # SESSION VARIABLES
  home.sessionVariables = {
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
    wezterm firefox thunar
    nodejs python3 go rustup gcc gnumake cmake
    ripgrep fd bat eza dust duf procs bottom fastfetch
    imv mpv wl-clipboard cliphist grim slurp swappy wf-recorder
    pavucontrol libnotify unzip p7zip tree calc
    iosevka jetbrains-mono sourceHighlight imagemagick
    xarchiver nixd antigravity
  ];

  # XDG
  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
}
