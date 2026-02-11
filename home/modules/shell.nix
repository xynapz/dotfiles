# Shell Configuration (Bash + Oh My Bash)
{ config, pkgs, lib, ... }:

{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    historySize = 50000;
    historyFileSize = 100000;
    historyControl = [ "ignoreboth" "erasedups" ];
    historyIgnore = [ "ls" "cd" "cd -" "pwd" "exit" "clear" "history" "bg" "fg" ];
    shellOptions = [ "histappend" "cmdhist" "autocd" "cdspell" "dirspell" "globstar" "nocaseglob" "checkwinsize" "extglob" ];

    shellAliases = {
      # Modern ls via eza (with icons and git integration)
      ls = "eza --icons --group-directories-first";
      ll = "eza -la --icons --git --group-directories-first";
      la = "eza -a --icons --group-directories-first";
      lt = "eza -la --icons --git --sort=modified";
      l = "eza --icons";
      tree = "eza --tree --icons -I '.git|node_modules|__pycache__|.venv|target|build'";

      # Cat/Less with syntax highlighting
      cat = "bat --style=plain";
      less = "bat --style=plain --paging=always";

      # Help
      help = "tldr";
      "?" = "tldr";

      # Navigation
      ".." = "cd .."; "..." = "cd ../.."; "...." = "cd ../../.."; "....." = "cd ../../../..";

      # Safety
      cp = "cp -i"; mv = "mv -i"; rm = "rm -i";

      # Grep with color
      grep = "grep --color=auto";
      fgrep = "fgrep --color=auto";
      egrep = "egrep --color=auto";
      diff = "diff --color=auto";

      # Python
      penv = "source ./.venv/bin/activate";
      py = "python3";

      # Emacs (via daemon - instant startup)
      e = "emacsclient -c -a ''";
      et = "emacsclient -nw -a ''";
      ds = "doom sync";
      emd = "emacs --daemon";
      emk = "pkill -f 'emacs --daemon'";

      # NixOS
      nrs = "sudo nixos-rebuild switch --flake ~/dotfiles#xynapz";
      nrb = "sudo nixos-rebuild boot --flake ~/dotfiles#xynapz";

      # Misc tools
      df = "duf";
      du = "dust";
      ps = "procs";
      top = "btm";
      fetch = "fastfetch";
      md = "glow";
      bench = "hyperfine";
      loc = "tokei";
    };

    initExtra = ''
      # Oh My Bash (auto-install if missing)
      export OSH="$HOME/.oh-my-bash"

      if [[ ! -d "$OSH" ]]; then
        echo "Installing Oh My Bash..."
        git clone --depth 1 https://github.com/ohmybash/oh-my-bash.git "$OSH" 2>/dev/null
      fi

      OSH_THEME="pzq"

      plugins=(
        git
        bashmarks
        progress
      )

      completions=(
        git
        ssh
        system
      )

      aliases=()

      if [[ -f "$OSH/oh-my-bash.sh" ]]; then
        source "$OSH/oh-my-bash.sh"
      fi

      # Colored man pages
      export LESS_TERMCAP_mb=$'\e[1;32m'
      export LESS_TERMCAP_md=$'\e[1;36m'
      export LESS_TERMCAP_me=$'\e[0m'
      export LESS_TERMCAP_se=$'\e[0m'
      export LESS_TERMCAP_so=$'\e[01;44;33m'
      export LESS_TERMCAP_ue=$'\e[0m'
      export LESS_TERMCAP_us=$'\e[1;35m'
      export GROFF_NO_SGR=1

      # Functions
      mkcd() { mkdir -p "$1" && cd "$1"; }

      extract() {
        [ -f "$1" ] || { echo "'$1' is not a valid file"; return; }
        case "$1" in
          *.tar.bz2) tar xjf "$1" ;; *.tar.gz) tar xzf "$1" ;; *.tar.xz) tar xJf "$1" ;;
          *.bz2) bunzip2 "$1" ;; *.gz) gunzip "$1" ;; *.tar) tar xf "$1" ;;
          *.zip) unzip "$1" ;; *.7z) 7z x "$1" ;; *) echo "'$1' cannot be extracted" ;;
        esac
      }

      ff() { fd --type f "$1" 2>/dev/null || find . -type f -iname "*$1*" 2>/dev/null; }
      fdir() { fd --type d "$1" 2>/dev/null || find . -type d -iname "*$1*" 2>/dev/null; }

      up() {
        local count="''${1:-1}"; local path=""
        for ((i=0;i<count;i++)); do path="../$path"; done
        cd "$path" || return
      }

      preview() { bat --style=numbers --color=always "''${1:--}" | head -''${2:-100}; }
    '';

    profileExtra = ''
      [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
    '';
  };

  # Explicitly disable starship (was previously enabled)
  programs.starship.enable = false;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "inode/directory" = [ "org.gnome.Nautilus.desktop" ];
      "x-scheme-handler/terminal" = [ "org.wezfurlong.wezterm.desktop" ];
    };
  };

  programs.direnv = { enable = true; enableBashIntegration = true; nix-direnv.enable = true; };
  programs.fzf = { enable = true; enableBashIntegration = true; defaultOptions = [ "--height 40%" "--layout=reverse" "--border" ]; };
  programs.zoxide = { enable = true; enableBashIntegration = true; };
}
