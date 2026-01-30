# Doom Emacs Configuration
{ config, pkgs, lib, ... }:

{
  programs.emacs = { enable = true; package = pkgs.emacs; extraPackages = epkgs: [ epkgs.vterm ]; };

  home.file.".doom.d" = { source = ../../config/doom; recursive = true; };

  home.packages = with pkgs; [
    git ripgrep fd coreutils clang
    nodePackages.typescript-language-server nodePackages.vscode-langservers-extracted nodePackages.bash-language-server
    pyright gopls clang-tools terraform-ls yaml-language-server lua-language-server
    nodePackages.prettier black ruff stylua shfmt nixpkgs-fmt
    aspell aspellDicts.en aspellDicts.en-computers
  ];

  services.emacs = { enable = true; defaultEditor = false; startWithUserSession = "graphical"; };
}
