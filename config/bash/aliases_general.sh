# Navigation, safety, and general aliases
# Sourced by shell.nix

# Modern ls via eza
alias ls='eza --icons=auto --group-directories-first'
alias ll='eza -la --icons=auto --git --group-directories-first'
alias la='eza -a --icons=auto --group-directories-first'
alias lt='eza -la --icons=auto --git --sort=modified'
alias l='eza --icons=auto'
alias tree='eza --tree --icons=auto -I ".git|node_modules|__pycache__|.venv|target|build"'

# Cat/Less with syntax highlighting
alias cat='bat --style=plain'
alias less='bat --style=plain --paging=always'

# Help
alias help='tldr'
alias '?'='tldr'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Safety
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Grep with color
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'

# Modern tool replacements
alias df='duf'
alias du='dust'
alias ps='procs'
alias top='btm'
alias fetch='fastfetch'
alias md='glow'
alias bench='hyperfine'
alias loc='tokei'
