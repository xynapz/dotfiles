# Common config
export ZSH="$HOME/.oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"
ZSH_THEME="arrow"
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

# Editor
export EDITOR='vim'
export VISUAL='vim'
export PAGER='cat'

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt SHARE_HISTORY

# Autosuggestions style
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
bindkey '^ ' autosuggest-accept

export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin
export PATH="$HOME/zig/zig-linux-x86_64-0.14.0:$PATH"
export PATH="$PATH:$HOME/dotfiles/scripts/path"


# Aliases and functions (common)
alias xc="xclip -selection clipboard"
alias ta="tmux attach -t"

source $HOME/.zshrc_fn

