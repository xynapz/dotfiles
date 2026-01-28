if [[ "$INSIDE_EMACS" ]]; then
    export TERM=foot
fi
export TERM=foot
export PATH="$HOME/dotfiles/scripts/path:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export LESSOPEN="||/usr/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R'
