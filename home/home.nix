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
    EDITOR = "emacsclient -nw -a ''";      # Terminal emacs via daemon (instant)
    VISUAL = "emacsclient -c -a ''";       # GUI emacs via daemon (instant)
    PAGER = "bat --style=plain --paging=always";  # Syntax highlighting everywhere
    LESS = "-R -i -M -S -x4";  # Still used by programs that explicitly call less
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";  # Colored man pages
    BAT_THEME = "Nord";  # Consistent theme
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
    home-manager ffmpeg
    wezterm firefox texliveFull poppler-utils
    nodejs python3 go rustup gcc gnumake cmake
    ripgrep fd bat eza dust duf procs bottom fastfetch
    tealdeer          # tldr - simplified man pages
    jq yq             # JSON/YAML processing
    httpie            # Modern HTTP client (http/https commands)
    tokei             # Code statistics
    hyperfine         # Command benchmarking
    glow              # Markdown viewer in terminal
    ncdu              # Interactive disk usage
    wl-clipboard cliphist grim slurp swappy wf-recorder
    pavucontrol libnotify unzip p7zip tree calc
    iosevka jetbrains-mono sourceHighlight imagemagick
    xarchiver nixd antigravity discord sddm-astronaut
    gnome-calculator gnome-pomodoro nautilus morgen
    gedit evince eog gcolor3 yt-dlp yt-dlg rhythmbox
    eww
  ];

  # XDG
  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
}
