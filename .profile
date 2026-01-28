# .profile

# -- Environment Variables --
export EDITOR="emacs -nw"
export VISUAL="emacs -nw"
export LESS=' -R'
export LESSOPEN="||/usr/bin/src-hilite-lesspipe.sh %s"

# Load the .bashrc file for wezterm
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# -- PATH Management --
# Helper to prevent duplicate path entries
add_to_path() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1:$PATH"
    fi
}

# Prepend user directories to PATH (reverse order of priority)
add_to_path "$HOME/.emacs.d/bin"
add_to_path "$HOME/go/bin"
add_to_path "$HOME/dotfiles/scripts/path"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/bin"

unset -f add_to_path

# -- Language Environments --

# Cargo/Rust
if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
fi

export PATH
