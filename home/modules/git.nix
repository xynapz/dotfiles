# Git Configuration
{ config, pkgs, lib, ... }:

{
  programs.git = {
    enable = true;
    userName = "xynapz";
    userEmail = "xynapz@aol.com";
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "emacs -nw";
      pull.rebase = true; push.autoSetupRemote = true; fetch.prune = true;
    };
    delta = { enable = true; options = { navigate = true; side-by-side = true; line-numbers = true; syntax-theme = "Nord"; }; };
    ignores = [ ".DS_Store" "*~" "*.swp" ".idea/" ".vscode/" "__pycache__/" "*.py[cod]" ".venv/" "node_modules/" "build/" "dist/" "target/" "*.log" ];
    aliases = { st = "status -sb"; lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"; };
  };
  programs.gh = { enable = true; settings = { git_protocol = "ssh"; }; };
}
