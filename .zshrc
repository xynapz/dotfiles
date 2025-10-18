# Common config
export ZSH="$HOME/.oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"
ZSH_THEME="arrow"
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

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

# Platform-specific configs
case "$OSTYPE" in
  darwin*)
    export PATH="/opt/homebrew/bin:$PATH"
    ;;
  linux*)
    export PATH=$PATH:/usr/local/go/bin
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin
    export PATH="$HOME/zig/zig-linux-x86_64-0.14.0:$PATH"
    export DXSCRIPTS="/home/$USER/dotfiles/scripts/"
    export PATH="$PATH:$DXSCRIPTS"
    export PATH="$PATH:$HOME/.config/emacs/bin"
    ;;
esac

# Editor
export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'

# Aliases and functions (common)
alias dcu="docker compose up -d"
alias dcd="docker compose down"
alias grep='grep --color=auto'
alias cdwm='cd ~/suckless/dwm/'
alias vdwm='vim ~/suckless/dwm/'
alias mdwm='cd ~/suckless/dwm/; sudo make clean install; cd -'
alias de='EMACSLOADPATH=$HOME/.config/emacs/bin emacs --init-directory=~/.config/doom/'
alias xc="xclip -selection clipboard"
alias ta="tmux attach -t"
#default browser
export BROWSER=zen-browser

wifi-doc() {
    doc="
List    => nmcli device wifi list
Rescan  => nmcli device wifi rescan
Connect => nmcli device wifi connect access_point_name password your_password
Show    => nmcli connection show
"
echo "$doc"
}

# Neovim cleanup function
nvim-clean() {
  echo "Cleaning Neovim and Lazy.nvim caches and packages..."
  # Define Neovim and Lazy.nvim directories
  NVIM_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/nvim"
  NVIM_STATE="${XDG_STATE_HOME:-$HOME/.local/state}/nvim"
  NVIM_DATA="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
  # Lazy.nvim usually stores plugins here
  LAZY_DIR="$NVIM_DATA/lazy"
  # Remove caches
  echo "Removing Neovim cache: $NVIM_CACHE"
  rm -rf "$NVIM_CACHE"
  # Remove Lazy.nvim installed plugins
  echo "Removing Lazy.nvim plugins: $LAZY_DIR"
  rm -rf "$LAZY_DIR"
  # Optionally remove Neovim state and data uncomment
  echo "Removing Neovim state: $NVIM_STATE"
  rm -rf "$NVIM_STATE"
  # echo "Removing Neovim data (excluding plugins): $NVIM_DATA"
  # rm -rf "$NVIM_DATA"
  echo "Cleanup complete ✅"
  cd -
}

#open neovim config in nvim
ncf() {
  echo "Opening Neovim Config"
  cd ~/.config/nvim/
  nvim
  cd -
}

rmvol(){
  echo "Listing all the docker volumes.."
  #store the volumes to an array
  volumes=($(docker volume ls --format '{{.Name}}'))

  #check for no volumes
  if [ ${#volumes[@]} -eq 0 ]; then
    echo "No Volumes Found"
    return
  fi

  #show volumes list with index
  for i in $(seq 1 ${#volumes[@]}); do
    echo "$i) ${volumes[$((i))]}"
  done

  # Prompt user for input
  echo ""
  echo "Enter the number of the volume to delete, or 'q' to quit:"
  read choice

  # Check if user wants to quit
  if [ "$choice" = "q" ] || [ "$choice" = "Q" ]; then
    echo "Operation cancelled."
    return
  fi

  # Validate input is a number
  if ! [[ "$choice" =~ ^[0-9]+$ ]]; then
    echo "Invalid input. Please enter a number."
    return
  fi

  # Validate the choice is within range
  if [ "$choice" -lt 1 ] || [ "$choice" -gt ${#volumes[@]} ]; then
    echo "Invalid selection. Number must be between 1 and ${#volumes[@]}."
    return
  fi

  # retrieve the name of volume to delete from index
  volume_to_remove="${volumes[$((choice))]}"

  # Confirm before deletion
  echo "Are you sure you want to delete volume '$volume_to_remove'? (y/n)"
  read confirm

  if [ "$confirm" = "y" ] || [ "$confirm" = "Y" ]; then
    echo "removing volume: $volume_to_remove"
    docker volume rm "$volume_to_remove"
    if [ $? -eq 0 ]; then
      echo "volume removed successfully"
    else
      echo "failed to remove volume"
    fi
  else
    echo "operation failed"
  fi
}



export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
