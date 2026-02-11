# Git aliases and functions
# Sourced by shell.nix

# Status & Info
alias gs='git status -sb'
alias gl='git log --oneline -20'
alias glg='git log --graph --oneline --decorate -20'
alias gd='git diff'
alias gds='git diff --staged'
alias gbl='git blame -b -w'

# Staging & Committing
alias ga='git add .'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend --no-edit'

# Branches
alias gco='git checkout'
alias gcb='git checkout -b'
alias gb='git branch'
alias gbd='git branch -d'

# Remote
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpl='git pull --rebase'
alias gf='git fetch --all --prune'

# Stash & Rebase
alias gst='git stash'
alias gstp='git stash pop'
alias grb='git rebase'
alias grbi='git rebase -i'
alias grs='git reset'
alias grsh='git reset --hard'
alias gcp='git cherry-pick'
