# .profile
# Environment variables, PATH, and login shell config
# Sourced by: .bash_profile (login shells)

# Editor
export EDITOR="emacs -nw"
export VISUAL="emacs -nw"

# Pager & Syntax Highlighting
export PAGER="less"
export LESS='-R -i -M -S -x4'
# -R: Raw control chars (for colors)
# -i: Case-insensitive search
# -M: Long prompt
# -S: Chop long lines
# -x4: Tab stops at 4

# Source-highlight integration
if [ -x /usr/bin/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
elif [ -x /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
fi

# Colored man pages (using less termcap)
export LESS_TERMCAP_mb=$'\e[1;31m'      # Begin bold
export LESS_TERMCAP_md=$'\e[1;36m'      # Begin blink (headers)
export LESS_TERMCAP_me=$'\e[0m'         # End mode
export LESS_TERMCAP_so=$'\e[1;44;33m'   # Begin standout (search)
export LESS_TERMCAP_se=$'\e[0m'         # End standout
export LESS_TERMCAP_us=$'\e[1;32m'      # Begin underline
export LESS_TERMCAP_ue=$'\e[0m'         # End underline

# PATH Management
add_to_path() {
    [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]] && PATH="$1:$PATH"
}

add_to_path "$HOME/.emacs.d/bin"
add_to_path "$HOME/go/bin"
add_to_path "$HOME/dotfiles/scripts/path"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/bin"

unset -f add_to_path
export PATH

# Language Environments

# Cargo/Rust
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Source .bashrc for interactive shells
[ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
