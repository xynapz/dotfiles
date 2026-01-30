# .bashrc
# Interactive shell configuration
# Sourced by: .profile (via .bash_profile for login shells)

# Global Definitions
[ -f /etc/bashrc ] && . /etc/bashrc

# Exit if not interactive
[[ $- != *i* ]] && return

# SHELL OPTIONS

# History
HISTSIZE=10000
HISTFILESIZE=20000
HISTCONTROL=ignoreboth:erasedups
HISTIGNORE="ls:cd:cd -:pwd:exit:clear:history"
shopt -s histappend
shopt -s cmdhist # Multi-line commands as single entry

# Navigation
shopt -s autocd   # cd into directories by name
shopt -s cdspell  # Correct minor typos in cd
shopt -s dirspell # Correct directory spelling

# Globbing
shopt -s globstar   # ** matches nested directories
shopt -s nocaseglob # Case-insensitive globbing

# Other
shopt -s checkwinsize # Update LINES/COLUMNS after each command

# ALIASES

# Safety & Defaults
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Listing
alias ll='ls -lh'
alias la='ls -lAh'
alias l='ls -CF'
alias lt='ls -lhtr' # Sort by time, newest last

# Tools
alias penv='source ./.venv/bin/activate'
alias ta='tmux attach -t'
alias em='emacs -nw'
alias tree="tree -I '.git|node_modules|vendor|__pycache__|.venv|target|build|dist'"
alias ds="doom sync"
alias emk="pkill -f emacs"
alias emd="emacs --daemon"

# Git (common shortcuts)
alias gs='git status -sb'
alias gl='git log --oneline -20'
alias gd='git diff'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gf='git fetch --all --prune'

# FUNCTIONS

# mkcd - Create directory and cd into it
mkcd() {
    mkdir -p "$1" && cd "$1"
}

# extract - Universal archive extractor
extract() {
    if [ -f "$1" ]; then
        case "$1" in
        *.tar.bz2) tar xjf "$1" ;;
        *.tar.gz) tar xzf "$1" ;;
        *.tar.xz) tar xJf "$1" ;;
        *.bz2) bunzip2 "$1" ;;
        *.rar) unrar x "$1" ;;
        *.gz) gunzip "$1" ;;
        *.tar) tar xf "$1" ;;
        *.tbz2) tar xjf "$1" ;;
        *.tgz) tar xzf "$1" ;;
        *.zip) unzip "$1" ;;
        *.Z) uncompress "$1" ;;
        *.7z) 7z x "$1" ;;
        *) echo "'$1' cannot be extracted" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# ccat - Cat with syntax highlighting (using source-highlight)
ccat() {
    if [ -f "$1" ]; then
        source-highlight --failsafe -f esc -i "$1" 2>/dev/null || cat "$1"
    else
        cat "$@"
    fi
}

# ff - Find file by name (classic find wrapper)
ff() {
    find . -type f -iname "*$1*" 2>/dev/null
}

# fd - Find directory by name
fdir() {
    find . -type d -iname "*$1*" 2>/dev/null
}

# up - Go up N directories
up() {
    local count="${1:-1}"
    local path=""
    for ((i = 0; i < count; i++)); do
        path="../$path"
    done
    cd "$path" || return
}

# OH-MY-BASH

export OSH='/home/xynapz/.oh-my-bash'
OSH_THEME="pzq"
OMB_USE_SUDO=true

completions=(
    git
    ssh
)

aliases=(
    general
)

plugins=(
    git
    sudo
    bashmarks
    colored-man-pages
)

source "$OSH/oh-my-bash.sh"
