# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ $HOME/.local/bin:$HOME/bin: ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

alias penv="source ./.venv/bin/activate"
alias ta="tmux attach -t"
alias em="emacs -nw"
alias tree="tree -I '.git|.hg|.svn|.DS_Store|auto|node_modules|vendor|build|dist|out|obj|target|.cache|__pycache__|venv|.venv|env|cmake-build-debug|cmake-build-release|elpa|.idea|.vscode|.next|.nuxt|coverage|tmp|temp|eln-cache'"
. "$HOME/.cargo/env"