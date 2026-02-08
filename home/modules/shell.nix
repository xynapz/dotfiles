# Shell Configuration (Bash + Starship)
{ config, pkgs, lib, ... }:

{
  programs.bash = {
    enable = true;
    historySize = 10000;
    historyFileSize = 20000;
    historyControl = [ "ignoreboth" "erasedups" ];
    historyIgnore = [ "ls" "cd" "cd -" "pwd" "exit" "clear" "history" ];
    shellOptions = [ "histappend" "cmdhist" "autocd" "cdspell" "dirspell" "globstar" "nocaseglob" "checkwinsize" ];

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

      # Git
      gs = "git status -sb"; gl = "git log --oneline -20"; gd = "git diff";
      ga = "git add"; gc = "git commit"; gp = "git push"; gf = "git fetch --all --prune";
      gco = "git checkout"; gb = "git branch";

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
      # Colored man pages (LESS_TERMCAP) - makes man pages beautiful
      export LESS_TERMCAP_mb=$'\e[1;32m'      # begin blink (green)
      export LESS_TERMCAP_md=$'\e[1;36m'      # begin bold (cyan)
      export LESS_TERMCAP_me=$'\e[0m'         # end mode
      export LESS_TERMCAP_se=$'\e[0m'         # end standout
      export LESS_TERMCAP_so=$'\e[01;44;33m'  # standout (yellow on blue)
      export LESS_TERMCAP_ue=$'\e[0m'         # end underline
      export LESS_TERMCAP_us=$'\e[1;35m'      # begin underline (magenta)
      export GROFF_NO_SGR=1                   # for older groff versions

      # Utility functions
      mkcd() { mkdir -p "$1" && cd "$1"; }

      extract() {
        [ -f "$1" ] || { echo "'$1' is not a valid file"; return; }
        case "$1" in
          *.tar.bz2) tar xjf "$1" ;; *.tar.gz) tar xzf "$1" ;; *.tar.xz) tar xJf "$1" ;;
          *.bz2) bunzip2 "$1" ;; *.gz) gunzip "$1" ;; *.tar) tar xf "$1" ;;
          *.zip) unzip "$1" ;; *.7z) 7z x "$1" ;; *) echo "'$1' cannot be extracted" ;;
        esac
      }

      # Search functions
      ff() { fd --type f "$1" 2>/dev/null || find . -type f -iname "*$1*" 2>/dev/null; }
      fdir() { fd --type d "$1" 2>/dev/null || find . -type d -iname "*$1*" 2>/dev/null; }

      # Directory navigation
      up() {
        local count="''${1:-1}"; local path=""
        for ((i=0;i<count;i++)); do path="../$path"; done
        cd "$path" || return
      }

      # Preview file with syntax highlighting
      preview() { bat --style=numbers --color=always "$1" | head -100; }

      # Quick JSON pretty print
      json() { echo "$1" | jq .; }
    '';

    profileExtra = ''
      [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
    '';
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      # Classic two-line prompt with elegant styling
      format = lib.concatStrings [
        "[┌─](bold white)"
        "$username"
        "$hostname"
        "$directory"
        "$git_branch"
        "$git_status"
        "$nix_shell"
        "$python"
        "$nodejs"
        "$rust"
        "$golang"
        "$cmd_duration"
        "$line_break"
        "[└─](bold white)$character"
      ];

      add_newline = true;

      # Character (prompt symbol on second line)
      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[✗](bold red)";
        vimcmd_symbol = "[❮](bold green)";
      };

      # Username
      username = {
        show_always = false;
        style_user = "bold bg:blue fg:black";
        style_root = "bold bg:red fg:white";
        format = "[ $user ]($style)";
      };

      # Hostname
      hostname = {
        ssh_only = true;
        format = "[@$hostname ]($style)";
        style = "bold bg:green fg:black";
      };

      # Directory with box styling
      directory = {
        truncation_length = 3;
        truncate_to_repo = true;
        style = "bold bg:cyan fg:black";
        format = "[ $path ]($style)[$read_only]($read_only_style)";
        read_only = " 󰌾";
        read_only_style = "bold bg:red fg:white";
        truncation_symbol = "…/";
      };

      # Git Branch with enhanced styling
      git_branch = {
        symbol = "";
        style = "bold bg:purple fg:black";
        format = "[ $symbol $branch ]($style)";
      };

      # Git Status with detailed indicators
      git_status = {
        style = "bold bg:yellow fg:black";
        format = "([ $all_status$ahead_behind]($style))";
        conflicted = "🏳 ";
        ahead = "⇡\${count} ";
        behind = "⇣\${count} ";
        diverged = "⇕⇡\${ahead_count}⇣\${behind_count} ";
        untracked = "?\${count} ";
        stashed = "$\${count} ";
        modified = "!\${count} ";
        staged = "+\${count} ";
        renamed = "»\${count} ";
        deleted = "✘\${count} ";
      };

      # Nix Shell with box styling
      nix_shell = {
        symbol = "";
        style = "bold bg:blue fg:white";
        format = "[ $symbol nix ]($style)";
        impure_msg = "";
        pure_msg = "";
      };

      # Programming Languages with box styling
      python = {
        symbol = "";
        style = "bold bg:yellow fg:black";
        format = "[ \${symbol} \${pyenv_prefix}(\${version} )(\\($virtualenv\\) )]($style)";
      };

      nodejs = {
        symbol = "";
        style = "bold bg:green fg:black";
        format = "[ $symbol ($version) ]($style)";
      };

      rust = {
        symbol = "";
        style = "bold bg:red fg:white";
        format = "[ $symbol ($version) ]($style)";
      };

      golang = {
        symbol = "";
        style = "bold bg:cyan fg:black";
        format = "[ $symbol ($version) ]($style)";
      };

      # Command Duration with styling
      cmd_duration = {
        min_time = 500;
        style = "bold bg:white fg:black";
        format = "[ 󱦟 $duration ]($style)";
      };
    };
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
