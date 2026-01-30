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
      add_newline = true;
      character = { success_symbol = "[❯](bold green)"; error_symbol = "[❯](bold red)"; };
      directory = { truncation_length = 3; style = "bold cyan"; };
      git_branch = { symbol = " "; style = "bold purple"; };
      nix_shell = { symbol = " "; style = "bold blue"; };
    };
  };

  programs.direnv = { enable = true; enableBashIntegration = true; nix-direnv.enable = true; };
  programs.fzf = { enable = true; enableBashIntegration = true; defaultOptions = [ "--height 40%" "--layout=reverse" "--border" ]; };
  programs.zoxide = { enable = true; enableBashIntegration = true; };
}
