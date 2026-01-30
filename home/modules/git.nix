# Git Configuration
{ config, pkgs, lib, ... }:

{
  programs.git = {
    enable = true;
    settings = {
      user.name = "xynapz";
      user.email = "xynapz@aol.com";
      
      init.defaultBranch = "main";
      core.editor = "emacs -nw";
      pull.rebase = true; push.autoSetupRemote = true; fetch.prune = true;
      aliases = { 
        st = "status -sb"; 
        lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"; 
      };
    };
    
    ignores = [ ".DS_Store" "*~" "*.swp" ".idea/" ".vscode/" "__pycache__/" "*.py[cod]" ".venv/" "node_modules/" "build/" "dist/" "target/" "*.log" ];
  };

  programs.delta = { enable = true; options = { navigate = true; side-by-side = true; line-numbers = true; syntax-theme = "Nord"; }; };
  programs.gh = { enable = true; settings = { git_protocol = "ssh"; }; };
}
