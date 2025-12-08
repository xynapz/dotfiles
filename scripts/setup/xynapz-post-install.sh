#!/bin/bash

# XYNAPZ ARCH LINUX BOOTSTRAP
# Author: Generated for Xynapz
# Hardware: Asus Vivobook 15 (Ryzen 5000 / Radeon)
# Features: Hyprland, SDDM, SSH, Hyprlock, Emacs, Graphics Driver, Steam, OH-MY-ZSH

# Stop script on error
set -e

# --- Configuration ---
DOTFILES_REPO="https://github.com/xynapz/dotfiles.git"
EMACS_REPO="https://github.com/xynapz/.emacs.d.git"
DOTFILES_DIR="$HOME/dotfiles"
LOG_FILE="$HOME/install_log.txt"

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${BLUE}[INFO]${NC} $1"; echo "[INFO] $1" >> "$LOG_FILE"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; echo "[OK] $1" >> "$LOG_FILE"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; echo "[ERROR] $1" >> "$LOG_FILE"; exit 1; }

prepare_system() {
    log "Updating system keyring and base packages..."
    sudo pacman -Sy --noconfirm archlinux-keyring
    sudo pacman -Syu --noconfirm base-devel git curl wget
}

install_yay() {
    if command -v yay &> /dev/null; then
        success "Yay is already installed."
    else
        log "Installing Yay..."
        cd /tmp
        rm -rf yay
        git clone https://aur.archlinux.org/yay.git
        cd yay
        makepkg -si --noconfirm
        cd "$HOME"
        success "Yay installed."
    fi
}

generate_pkg_list() {
    cat <<EOF > "$HOME/packages.ini"
# Core Utils
zsh
gcc
clang
base-devel
gdb
emacs
git
openssh
fzf
ripgrep
fd
htop
unzip
zip
thunar
brightnessctl
dmidecode
nodejs
typescript
python-pip
npm
pnpm
tree
tmux
vim

# Hyprland Desktop
hyprland
waybar
rofi-wayland
swaync
wlogout
slurp
grim
swappy
wl-clipboard
ghostty
swww
swayosd

# Login & Locking
sddm
sddm-sugar-candy-git
qt5-graphicaleffects
qt5-quickcontrols2
qt5-svg
qt5-declarative
hyprlock
hypridle

# Fonts
ttf-iosevka-nerd
ttf-jetbrains-mono-nerd
ttf-nerd-fonts-symbols            # M-x nerd-icons-install-fonts
ttf-font-awesome

# Fonts for most human languages
noto-fonts                       # European, Asian, Middle-Eastern, African, Latin Fonts
noto-fonts-cjk                   # Chinese, Japanese, Korean Fonts
noto-fonts-extra                 # Extras, Historical Fonts
noto-fonts-emoji

# Audio
pipewire
pipewire-pulse
pipewire-alsa
wireplumber

# latex, org, pdf-tools dependencies (tex namespaces changed in 2025 update)
texlive-basic
texlive-latex
texlive-latexextra
texlive-plaingeneric
texlive-xetex
texlive-fontsrecommended
poppler                          # M-x pdf-tools-install to manual build.
poppler-glib

# Other tools
cmake
libtool
fastfetch
discord
google-chrome
nwg-displays
pyright
typescript-language-server
astrojs-language-server
EOF
}

install_packages() {
    log "Generating package list..."
    generate_pkg_list

    # Clean comments and empty lines
    PACKAGES=$(grep -vE "^\s*#" "$HOME/packages.ini" | sed 's/#.*//' | tr '\n' ' ')

    log "Installing packages using Yay..."
    yay -S --needed --noconfirm $PACKAGES
    success "Packages installed."
}

setup_omz() {
    OMZ_DIR="$HOME/.oh-my-zsh"
    OMZ_CUSTOM="$OMZ_DIR/custom"

    if [ -d "$OMZ_DIR" ]; then
        log "Oh-My-Zsh directory already exists."
    else
        log "Installing Oh-My-Zsh (Manual Clone)..."
        git clone https://github.com/ohmyzsh/ohmyzsh.git "$OMZ_DIR"
        success "Oh-My-Zsh core installed."
    fi

    if [ ! -d "$OMZ_CUSTOM/plugins/zsh-autosuggestions" ]; then
        log "Installing zsh-autosuggestions..."
        git clone https://github.com/zsh-users/zsh-autosuggestions "$OMZ_CUSTOM/plugins/zsh-autosuggestions"
        success "Plugin: zsh-autosuggestions installed."
    fi

    if [ ! -d "$OMZ_CUSTOM/plugins/zsh-syntax-highlighting" ]; then
        log "Installing zsh-syntax-highlighting..."
        git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$OMZ_CUSTOM/plugins/zsh-syntax-highlighting"
        success "Plugin: zsh-syntax-highlighting installed."
    fi
}

# --- 2.5 Setup GitHub SSH ---
setup_github_ssh() {
    log "Setting up GitHub SSH Authentication..."
    SSH_KEY="$HOME/.ssh/id_ed25519"

    if [ ! -f "$SSH_KEY" ]; then
        log "Generating new Ed25519 SSH key..."
        ssh-keygen -t ed25519 -C "angeldhakal97@gmail.com" -f "$SSH_KEY"
    else
        log "SSH Key found. Skipping generation."
    fi

    # Add to Agent (Current Session)
    eval "$(ssh-agent -s)" > /dev/null
    ssh-add "$SSH_KEY" 2>/dev/null

    # 3. Copy to Clipboard
    if command -v wl-copy &> /dev/null; then
        cat "${SSH_KEY}.pub" | wl-copy
        success "Public key copied to clipboard!"
    fi

    # Display & Pause
    echo -e "${YELLOW}====================================================${NC}"
    echo -e "${YELLOW}  ACTION REQUIRED: ADD KEY TO GITHUB  ${NC}"
    echo -e "${YELLOW}====================================================${NC}"
    echo -e "Key:"
    cat "${SSH_KEY}.pub"
    echo -e "\n1. Open: https://github.com/settings/keys"
    echo -e "2. Click 'New SSH Key'"
    echo -e "3. Paste the key (it is already in your clipboard)"
    echo -e "${YELLOW}====================================================${NC}"

    read -p "Press [Enter] once you have added the key to GitHub..."

    log "Testing GitHub connection..."
    if ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
        success "GitHub Authentication Verified!"
    else
        echo -e "${RED}[WARN] GitHub authentication failed. Please check your key.${NC}"
        sleep 3
    fi
}

setup_dotfiles() {
    log "Setting up Dotfiles..."

    if [ -d "$DOTFILES_DIR" ]; then
        log "Pulling latest changes..."
        git -C "$DOTFILES_DIR" pull
    else
        log "Cloning repo..."
        git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    fi

    link_file() {
        local path="$1"
        local src="$DOTFILES_DIR/$path"
        local dest="$HOME/$path"

        # Skip if source missing
        [ ! -e "$src" ] && return

        # Ensure parent directory exists (Critical for .config/foo)
        mkdir -p "$(dirname "$dest")"

        # Handle Existing Destinations
        if [ -L "$dest" ]; then
            unlink "$dest"
        elif [ -d "$dest" ]; then
            mv "$dest" "${dest}.bak.$(date +%s)"
            log "Backed up existing dir: $dest"
        elif [ -f "$dest" ]; then
            mv "$dest" "${dest}.bak.$(date +%s)"
        fi

        # Create the link
        ln -s "$src" "$dest"
        success "Linked $path"
    }
    # Root Files
    link_file ".zshrc"
    link_file ".zshrc_fn"
    link_file ".vimrc"
    link_file ".tmux.conf"
    link_file ".gitconfig"

    # Config Directories
    link_file ".config/ghostty"
    link_file ".config/hypr"
    link_file ".config/rofi"
    link_file ".config/swaync"
    link_file ".config/waybar"
    link_file ".config/wlogout"
}

setup_emacs() {
    log "Setting up Emacs..."
    if [ -d "$HOME/.emacs.d" ] && [ ! -d "$HOME/.emacs.d/.git" ]; then
         mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
    fi

    if [ -d "$HOME/.emacs.d" ]; then
        cd "$HOME/.emacs.d" && git pull
    else
        git clone "$EMACS_REPO" "$HOME/.emacs.d"
    fi
    success "Emacs setup complete."
}

setup_login() {
    log "Configuring SDDM..."

    if ! pacman -Qs sddm > /dev/null; then
        error "SDDM package missing. Please check packages.ini"
    fi

    if [ -d "/usr/share/sddm/themes/sugar-candy" ]; then
        THEME_NAME="sugar-candy"
    elif [ -d "/usr/share/sddm/themes/Sugar-Candy" ]; then
        THEME_NAME="Sugar-Candy"
    else
        echo -e "${YELLOW}[WARN] Sugar Candy theme folder not found. Is sddm-sugar-candy-git installed?${NC}"
        THEME_NAME="sugar-candy" # Try default anyway
    fi

    sudo mkdir -p /etc/sddm.conf.d

    printf "[Theme]\nCurrent=%s\n" "$THEME_NAME" | sudo tee /etc/sddm.conf.d/theme.conf > /dev/null

    if systemctl is-active --quiet gdm; then sudo systemctl disable --now gdm; fi
    if systemctl is-active --quiet lightdm; then sudo systemctl disable --now lightdm; fi

    sudo systemctl enable sddm
    success "SDDM enabled with theme: $THEME_NAME"
}

setup_steam() {
    log "Setting up Steam (Enabling Multilib & Installing Dependencies)..."

    # Enable Multilib Repository (Required for Steam/32-bit)
    # We check if the line starting with [multilib] exists. If not, we enable it.
    if ! grep -q "^\[multilib\]" /etc/pacman.conf; then
        log "Multilib repo not active. Enabling..."

        # Backup existing config just in case
        sudo cp /etc/pacman.conf /etc/pacman.conf.bak.$(date +%s)

        # Use sed to uncomment "[multilib]" and the "Include" line following it
        sudo sed -i "/\[multilib\]/,/Include/s/^#//" /etc/pacman.conf

        # Sync the database immediately so 'yay' can find the new packages
        log "Refreshing package database..."
        sudo pacman -Sy
        success "Multilib enabled and database refreshed."
    else
        log "Multilib repository is already enabled."
    fi

    packages=(
        # AMD GRAPHICS STACK (Stable)
        "mesa"
        "lib32-mesa"              # Required for Steam to see GPU
        "vulkan-radeon"           # Best Vulkan driver for Gaming
        "lib32-vulkan-radeon"     # 32-bit Vulkan for Steam
        "libva-mesa-driver"       # Video Acceleration
        "mesa-vdpau"

        # GAMING TOOLS
        "steam"
        "gamescope"               # Your upscaler tool
        "gamemode"                # CPU optimizer
        "lib32-gamemode"
        "vulkan-tools"            # Debugging tools (vkcube)

        # AUDIO & FONTS
        "lib32-pipewire"          # Fixes audio in 32-bit games
        "ttf-liberation"          # Fixes invisible text in Steam
    )

    log "Installing Steam and dependencies..."
    # I use pacman (not yay) to strictly force official, stable drivers
    sudo pacman -S --needed --noconfirm "${packages[@]}"

    # Ensure user can trigger gamemode
    sudo usermod -aG gamemode "$USER"

    success "Steam Setup Complete."
}

finalize() {
    log "Finalizing..."

    # Change Shell
    if [ "$(basename "$SHELL")" != "zsh" ]; then
        chsh -s "$(which zsh)"
    fi

    # add user to input group
    sudo usermod -aG input $USER

    # add user to video group
    sudo usermod -aG video $USER

    # Enable Services
    sudo systemctl enable --now NetworkManager
    sudo systemctl enable --now bluetooth

    # for special key input visual noti using swayosd
    sudo systemctl enable --now swayosd-libinput-backend.service

    # Disable Services
    sudo systemctl disable --now avahi-daemon

    # Reloading Fonts
    fc-cache -fv

    echo -e "${GREEN}=========================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE  ${NC}"
    echo -e "${GREEN}=========================================${NC}"
    echo "1. Reboot now to start SDDM > Hyprland."
    echo "2. Your packages are tracked in $HOME/packages.ini"
}

prepare_system
install_yay
install_packages
setup_omz
setup_github_ssh
setup_dotfiles
setup_emacs
setup_login
setup_steam
finalize
