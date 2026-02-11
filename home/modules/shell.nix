# Shell Configuration (Bash + Oh My Bash)
{
  config,
  pkgs,
  lib,
  ...
}:

let
  bashConfigDir = ../../config/bash;
in
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    historySize = 50000;
    historyFileSize = 100000;
    historyControl = [
      "ignoreboth"
      "erasedups"
    ];
    historyIgnore = [
      "ls"
      "cd"
      "cd -"
      "pwd"
      "exit"
      "clear"
      "history"
      "bg"
      "fg"
    ];
    shellOptions = [
      "histappend"
      "cmdhist"
      "autocd"
      "cdspell"
      "dirspell"
      "globstar"
      "nocaseglob"
      "checkwinsize"
      "extglob"
    ];

    initExtra = ''
      # Oh My Bash (auto-install if missing)
      export OSH="$HOME/.oh-my-bash"

      if [[ ! -d "$OSH" ]]; then
        echo "Installing Oh My Bash..."
        git clone --depth 1 https://github.com/ohmybash/oh-my-bash.git "$OSH" 2>/dev/null
      fi

      OSH_THEME="powerbash10k"

      plugins=(
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

      # Source custom config files
      source ${bashConfigDir}/aliases_general.sh
      source ${bashConfigDir}/aliases_git.sh
      source ${bashConfigDir}/aliases_projects.sh
      source ${bashConfigDir}/functions.sh

      # Colored man pages
      export LESS_TERMCAP_mb=$'\e[1;32m'
      export LESS_TERMCAP_md=$'\e[1;36m'
      export LESS_TERMCAP_me=$'\e[0m'
      export LESS_TERMCAP_se=$'\e[0m'
      export LESS_TERMCAP_so=$'\e[01;44;33m'
      export LESS_TERMCAP_ue=$'\e[0m'
      export LESS_TERMCAP_us=$'\e[1;35m'
      export GROFF_NO_SGR=1
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

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    defaultOptions = [
      "--height 40%"
      "--layout=reverse"
      "--border"
    ];
  };
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };
}
