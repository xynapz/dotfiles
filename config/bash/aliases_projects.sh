# Project-specific aliases (Emacs, Python, NixOS)
# Sourced by shell.nix

# Emacs (via daemon - instant startup)
alias e='emacsclient -c -a ""'
alias et='emacsclient -nw -a ""'
alias ds='doom sync'
alias emd='emacs --daemon'
alias emk='pkill -f "emacs --daemon"'

# Python
alias penv='source ./.venv/bin/activate'
alias py='python3'

# NixOS
alias nrs='sudo nixos-rebuild switch --flake ~/dotfiles#xynapz'
alias nrb='sudo nixos-rebuild boot --flake ~/dotfiles#xynapz'
