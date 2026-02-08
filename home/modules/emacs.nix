# Doom Emacs Configuration
{ config, pkgs, lib, ... }:

{
  programs.emacs = { enable = true; package = pkgs.emacs; extraPackages = epkgs: [ epkgs.vterm ]; };

  home.file.".doom.d" = { source = ../../config/doom; recursive = true; };

  home.packages = with pkgs; [
    # Core tools
    git ripgrep fd coreutils

    # LSP servers
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.bash-language-server
    pyright gopls clang-tools

    # Formatters
    nodePackages.prettier black ruff shfmt
    nixfmt  # doom doctor: nixfmt

    # Linting/checking (doom doctor requirements)
    shellcheck        # shell script linting
    html-tidy         # HTML formatting
    nodePackages.stylelint    # CSS linting
    nodePackages.js-beautify  # JS/CSS/HTML formatting

    # Python tools (doom doctor requirements)
    python312Packages.pyflakes  # import management
    python312Packages.isort     # import sorting
    python312Packages.pytest    # testing
    python312Packages.nose      # nosetests
    pipenv                      # pipenv support

    # Docker
    dockfmt                     # docker formatting

    # XML
    libxml2                     # xmllint

    # Markdown
    pandoc            # doom doctor: markdown compiler

    # Spell checking
    aspell aspellDicts.en aspellDicts.en-computers
  ];

  services.emacs = { enable = true; defaultEditor = false; startWithUserSession = "graphical"; };
}
