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
      ls = "ls --color=auto"; grep = "grep --color=auto"; diff = "diff --color=auto";
      fgrep = "fgrep --color=auto"; egrep = "egrep --color=auto";
      cp = "cp -i"; mv = "mv -i"; rm = "rm -i";
      ".." = "cd .."; "..." = "cd ../.."; "...." = "cd ../../..";
      ll = "ls -lh"; la = "ls -lAh"; l = "ls -CF"; lt = "ls -lhtr";
      cat = "bat --style=plain"; tree = "tree -I '.git|node_modules|__pycache__|.venv|target|build'";
      penv = "source ./.venv/bin/activate"; ta = "tmux attach -t";
      em = "emacs -nw"; ds = "doom sync"; emk = "pkill -f emacs"; emd = "emacs --daemon";
      gs = "git status -sb"; gl = "git log --oneline -20"; gd = "git diff";
      ga = "git add"; gc = "git commit"; gp = "git push"; gf = "git fetch --all --prune";
      nrs = "sudo nixos-rebuild switch --flake ~/dotfiles#xynapz";
      nrb = "sudo nixos-rebuild boot --flake ~/dotfiles#xynapz";
    };

    initExtra = ''
      mkcd() { mkdir -p "$1" && cd "$1"; }
      extract() {
        [ -f "$1" ] || { echo "'$1' is not a valid file"; return; }
        case "$1" in
          *.tar.bz2) tar xjf "$1" ;; *.tar.gz) tar xzf "$1" ;; *.tar.xz) tar xJf "$1" ;;
          *.bz2) bunzip2 "$1" ;; *.gz) gunzip "$1" ;; *.tar) tar xf "$1" ;;
          *.zip) unzip "$1" ;; *.7z) 7z x "$1" ;; *) echo "'$1' cannot be extracted" ;;
        esac
      }
      ff() { find . -type f -iname "*$1*" 2>/dev/null; }
      fdir() { find . -type d -iname "*$1*" 2>/dev/null; }
      up() { local count="''${1:-1}"; local path=""; for ((i=0;i<count;i++)); do path="../$path"; done; cd "$path" || return; }
      ccat() { [ -f "$1" ] && source-highlight --failsafe -f esc -i "$1" 2>/dev/null || cat "$@"; }
    '';

    profileExtra = ''
      [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
    '';
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      # Two-line prompt configuration
      format = lib.concatStrings [
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
        "$character"
      ];

      add_newline = true;

      # Character (prompt symbol on second line)
      character = {
        success_symbol = "[❯](bold green)";
        error_symbol = "[❯](bold red)";
        vimcmd_symbol = "[❮](bold green)";
      };

      # Username
      username = {
        show_always = false;
        style_user = "bold yellow";
        style_root = "bold red";
        format = "[$user]($style) ";
      };

      # Hostname
      hostname = {
        ssh_only = true;
        format = "[@$hostname]($style) ";
        style = "bold dimmed green";
      };

      # Directory
      directory = {
        truncation_length = 3;
        truncate_to_repo = true;
        style = "bold cyan";
        format = "[$path]($style)[$read_only]($read_only_style) ";
        read_only = " 󰌾";
        read_only_style = "red";
      };

      # Git Branch
      git_branch = {
        symbol = " ";
        style = "bold purple";
        format = "[$symbol$branch]($style) ";
      };

      # Git Status
      git_status = {
        style = "bold red";
        format = "([$all_status$ahead_behind]($style) )";
        conflicted = "🏳";
        ahead = "⇡\${count}";
        behind = "⇣\${count}";
        diverged = "⇕⇡\${ahead_count}⇣\${behind_count}";
        untracked = "?";
        stashed = "$";
        modified = "!";
        staged = "+";
        renamed = "»";
        deleted = "✘";
      };

      # Nix Shell
      nix_shell = {
        symbol = " ";
        style = "bold blue";
        format = "[$symbol$state]($style) ";
        impure_msg = "";
        pure_msg = "";
      };

      # Programming Languages
      python = {
        symbol = " ";
        style = "yellow";
        format = "[\${symbol}\${pyenv_prefix}(\${version} )(\\($virtualenv\\) )]($style)";
      };

      nodejs = {
        symbol = " ";
        style = "bold green";
        format = "[$symbol($version )]($style)";
      };

      rust = {
        symbol = " ";
        style = "bold red";
        format = "[$symbol($version )]($style)";
      };

      golang = {
        symbol = " ";
        style = "bold cyan";
        format = "[$symbol($version )]($style)";
      };

      # Command Duration
      cmd_duration = {
        min_time = 500;
        style = "bold yellow";
        format = "[took $duration]($style) ";
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
