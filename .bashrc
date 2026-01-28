# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# -- Aliases --

# Safety & Color
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Navigation
alias ..='cd ..'

# Tools
alias penv="source ./.venv/bin/activate"
alias ta="tmux attach -t"
alias em="emacs -nw"
alias tree="tree -I '.git|.hg|.svn|.DS_Store|auto|node_modules|vendor|build|dist|out|obj|target|.cache|__pycache__|venv|.venv|env|cmake-build-debug|cmake-build-release|elpa|.idea|.vscode|.next|.nuxt|coverage|tmp|temp|eln-cache'"

# -- Oh-My-Bash --
case $- in
*i*) ;;
*) return ;;
esac
export OSH='/home/xynapz/.oh-my-bash'
OSH_THEME="pzq"
OMB_USE_SUDO=true
# completions
completions=(
    git
    composer
    ssh
)
# aliases
aliases=(
    general
)
# plugins
plugins=(
    git
    bashmarks
    colored-man-pages
    fzf
)
# source oh-my-bash
source "$OSH"/oh-my-bash.sh
