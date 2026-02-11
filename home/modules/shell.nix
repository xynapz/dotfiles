# Shell Configuration (Bash + Starship + ble.sh)
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

      # Help system: tldr first, fallback to man
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
      e = "emacsclient -c -a ''";       # GUI emacs
      et = "emacsclient -nw -a ''";     # Terminal emacs
      ds = "doom sync";                 # Sync doom config
      emd = "emacs --daemon";           # Start daemon manually
      emk = "pkill -f 'emacs --daemon'"; # Kill daemon

      # NixOS
      nrs = "sudo nixos-rebuild switch --flake ~/dotfiles#xynapz";
      nrb = "sudo nixos-rebuild boot --flake ~/dotfiles#xynapz";

      # Misc tools
      df = "duf";                        # Prettier df
      du = "dust";                       # Prettier du
      ps = "procs";                      # Prettier ps
      top = "btm";                       # Bottom is better than top
      fetch = "fastfetch";               # System info
      md = "glow";                       # Markdown viewer
      bench = "hyperfine";               # Benchmarking
      loc = "tokei";                     # Code line counter
    };

    initExtra = ''
      # ── ble.sh (syntax highlighting, autosuggestions) ──
      if [[ -f "${pkgs.blesh}/share/blesh/ble.sh" ]] && [[ $- == *i* ]]; then
        source "${pkgs.blesh}/share/blesh/ble.sh" --noattach

        # Nord-themed syntax colors
        ble-face -s syntax_default           fg=252
        ble-face -s syntax_command           fg=cyan
        ble-face -s syntax_quoted            fg=green
        ble-face -s syntax_error             fg=red,bold
        ble-face -s syntax_comment           fg=242
        ble-face -s syntax_varname           fg=252
        ble-face -s syntax_expr              fg=magenta
        ble-face -s syntax_tilde             fg=cyan
        ble-face -s syntax_glob              fg=yellow
        ble-face -s filename_directory       fg=cyan,underline
        ble-face -s filename_executable      fg=green,bold
        ble-face -s auto_complete            fg=238
        ble-face -s region                   bg=60
        ble-face -s command_builtin          fg=cyan
        ble-face -s command_alias            fg=cyan
        ble-face -s command_function         fg=cyan,bold

        bleopt complete_auto_delay=100
        bleopt complete_auto_history=1
        bleopt complete_menu_style=dense
        bleopt highlight_syntax=1
        bleopt highlight_filename=1
      fi

      # ── Oh My Bash (auto-install if missing) ──
      export OSH="$HOME/.oh-my-bash"

      if [[ ! -d "$OSH" ]]; then
        echo "Installing Oh My Bash..."
        git clone --depth 1 https://github.com/ohmybash/oh-my-bash.git "$OSH" 2>/dev/null
      fi

      # Theme
      OSH_THEME="powerline-multiline"

      # Plugins
      plugins=(
        git
        bashmarks
        progress
      )

      # Completions
      completions=(
        git
        ssh
        system
      )

      # Aliases (we manage our own, disable OMB defaults)
      aliases=()

      # Load Oh My Bash
      if [[ -f "$OSH/oh-my-bash.sh" ]]; then
        source "$OSH/oh-my-bash.sh"
      fi

      # ── Colored man pages (LESS_TERMCAP) ──
      export LESS_TERMCAP_mb=$'\e[1;32m'
      export LESS_TERMCAP_md=$'\e[1;36m'
      export LESS_TERMCAP_me=$'\e[0m'
      export LESS_TERMCAP_se=$'\e[0m'
      export LESS_TERMCAP_so=$'\e[01;44;33m'
      export LESS_TERMCAP_ue=$'\e[0m'
      export LESS_TERMCAP_us=$'\e[1;35m'
      export GROFF_NO_SGR=1

      # ── Utility functions ──
      mkcd() { mkdir -p "$1" && cd "$1"; }

      extract() {
        [ -f "$1" ] || { echo "'$1' is not a valid file"; return; }
        case "$1" in
          *.tar.bz2) tar xjf "$1" ;; *.tar.gz) tar xzf "$1" ;; *.tar.xz) tar xJf "$1" ;;
          *.bz2) bunzip2 "$1" ;; *.gz) gunzip "$1" ;; *.tar) tar xf "$1" ;;
          *.zip) unzip "$1" ;; *.7z) 7z x "$1" ;; *) echo "'$1' cannot be extracted" ;;
        esac
      }

      # Search functions (uses fd with fallback)
      ff() { fd --type f "$1" 2>/dev/null || find . -type f -iname "*$1*" 2>/dev/null; }
      fdir() { fd --type d "$1" 2>/dev/null || find . -type d -iname "*$1*" 2>/dev/null; }

      # Directory navigation
      up() {
        local count="''${1:-1}"; local path=""
        for ((i=0;i<count;i++)); do path="../$path"; done
        cd "$path" || return
      }

      # Preview file with syntax highlighting
      preview() { bat --style=numbers --color=always "''${1:--}" | head -''${2:-100}; }

      # Quick JSON pretty print (pipe or argument)
      json() {
        if [ -t 0 ] && [ -n "''${1:-}" ]; then
          echo "$1" | jq .
        else
          jq .
        fi
      }

      # Git interactive log with fzf
      glog() {
        git log --oneline --graph --decorate --color=always | \
          fzf --ansi --no-sort --reverse --preview 'git show --color=always {1}' \
              --bind 'enter:execute(git show {1} | bat -l diff)+abort'
      }

      # ── Attach ble.sh at the end (must be last) ──
      [[ ''${BLE_VERSION-} ]] && ble-attach
    '';

    profileExtra = ''
      [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
    '';
  };

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
