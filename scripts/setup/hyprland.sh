#!/bin/bash

# Hyprland Setup Script for Arch Linux
# Production-grade installer with error handling and logging

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DOTFILES_DIR="$HOME/dotfiles/.config"
CONFIG_DIR="$HOME/.config"
LOG_FILE="/tmp/hyprland-setup-$(date +%Y%m%d-%H%M%S).log"

# Logging function
log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$LOG_FILE"
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$LOG_FILE"
}

# Error handler
error_exit() {
    log_error "$1"
    log_error "Installation failed. Check log at: $LOG_FILE"
    exit 1
}

# Check if running as root
if [[ $EUID -eq 0 ]]; then
   error_exit "This script should not be run as root. Run as your regular user."
fi

# Check if running Arch Linux
if [[ ! -f /etc/arch-release ]]; then
    error_exit "This script is designed for Arch Linux only."
fi

# Print banner
echo -e "${BLUE}"
cat << "EOF"
  ______   _______   __    __        __    __  __      __  _______   _______   __         ______   __    __  _______          ______   ________  ________  __    __  _______
 /      \ |       \ |  \  |  \      |  \  |  \|  \    /  \|       \ |       \ |  \       /      \ |  \  |  \|       \        /      \ |        \|        \|  \  |  \|       \
|  $$$$$$\| $$$$$$$\| $$  | $$      | $$  | $$ \$$\  /  $$| $$$$$$$\| $$$$$$$\| $$      |  $$$$$$\| $$\ | $$| $$$$$$$\      |  $$$$$$\| $$$$$$$$ \$$$$$$$$| $$  | $$| $$$$$$$\
| $$  | $$| $$__/ $$ \$$\/  $$      | $$__| $$  \$$\/  $$ | $$__/ $$| $$__| $$| $$      | $$__| $$| $$$\| $$| $$  | $$      | $$___\$$| $$__       | $$   | $$  | $$| $$__/ $$
| $$  | $$| $$    $$  >$$  $$       | $$    $$   \$$  $$  | $$    $$| $$    $$| $$      | $$    $$| $$$$\ $$| $$  | $$       \$$    \ | $$  \      | $$   | $$  | $$| $$    $$
| $$  | $$| $$$$$$$  /  $$$$\       | $$$$$$$$    \$$$$   | $$$$$$$ | $$$$$$$\| $$      | $$$$$$$$| $$\$$ $$| $$  | $$       _\$$$$$$\| $$$$$      | $$   | $$  | $$| $$$$$$$
| $$__/ $$| $$      |  $$ \$$\      | $$  | $$    | $$    | $$      | $$  | $$| $$_____ | $$  | $$| $$ \$$$$| $$__/ $$      |  \__| $$| $$_____    | $$   | $$__/ $$| $$
 \$$    $$| $$      | $$  | $$      | $$  | $$    | $$    | $$      | $$  | $$| $$     \| $$  | $$| $$  \$$$| $$    $$       \$$    $$| $$     \   | $$    \$$    $$| $$
  \$$$$$$  \$$       \$$   \$$       \$$   \$$     \$$     \$$       \$$   \$$ \$$$$$$$$ \$$   \$$ \$$   \$$ \$$$$$$$         \$$$$$$  \$$$$$$$$    \$$     \$$$$$$  \$$
EOF
echo -e "${NC}"
log "Starting Hyprland installation on Arch Linux"
log "Log file: $LOG_FILE"

# Verify dotfiles directory exists
if [[ ! -d "$DOTFILES_DIR" ]]; then
    error_exit "Dotfiles directory not found at $DOTFILES_DIR"
fi

log_info "Found dotfiles at $DOTFILES_DIR"

# Update system
log "Updating system packages..."
sudo pacman -Syu --noconfirm || error_exit "Failed to update system"

# Install base Hyprland packages
log "Installing Hyprland and core dependencies..."
HYPR_PACKAGES=(
    hyprland
    hyprlock
    hypridle
    xdg-desktop-portal-hyprland
    qt5-wayland
    qt6-wayland
    polkit-kde-agent
)


sudo pacman -S --needed --noconfirm "${HYPR_PACKAGES[@]}" || error_exit "Failed to install Hyprland packages"

# Install essential Wayland utilities
log "Installing Wayland utilities..."
WAYLAND_PACKAGES=(
    wayland
    wl-clipboard
    cliphist
    xorg-xwayland
)

sudo pacman -S --needed --noconfirm "${WAYLAND_PACKAGES[@]}" || error_exit "Failed to install Wayland utilities"

# Install terminal and file manager
log "Installing terminal and file manager..."
if ! sudo pacman -S --needed --noconfirm ghostty 2>/dev/null; then
    log_warning "Ghostty not available in repos - install from AUR with: yay -S ghostty"
fi

sudo pacman -S --needed --noconfirm dolphin konsole || error_exit "Failed to install file manager"

# Install Chrome
if ! command -v google-chrome-stable &> /dev/null; then
    log_warning "Google Chrome not found. Install from AUR with: yay -S google-chrome"
fi

# Install UI components
log "Installing UI components (waybar, rofi, etc.)..."
UI_PACKAGES=(
    waybar
    rofi-wayland
    dunst
    swaync
    swayidle
    swaylock
)

sudo pacman -S --needed --noconfirm "${UI_PACKAGES[@]}" || error_exit "Failed to install UI components"

# Install swww
log "Installing swww wallpaper daemon..."
if ! sudo pacman -S --needed --noconfirm swww 2>/dev/null; then
    log_warning "swww not in repos - install from AUR with: yay -S swww"
else
    log_info "swww installed successfully"
fi

# Install screenshot and screen recording tools
log "Installing screenshot and media tools..."
MEDIA_PACKAGES=(
    grim
    slurp
    swappy
    wf-recorder
    imagemagick
)

sudo pacman -S --needed --noconfirm "${MEDIA_PACKAGES[@]}" || error_exit "Failed to install media tools"

# Install audio and system utilities
log "Installing system utilities..."
SYSTEM_PACKAGES=(
    pipewire
    wireplumber
    pipewire-audio
    pipewire-pulse
    pavucontrol
    brightnessctl
    playerctl
    networkmanager
    bluez
    bluez-utils
    blueman
)

sudo pacman -S --needed --noconfirm "${SYSTEM_PACKAGES[@]}" || error_exit "Failed to install system utilities"

# Install fonts and themes
log "Installing fonts and themes..."
THEME_PACKAGES=(
    ttf-font-awesome
    ttf-jetbrains-mono-nerd
    noto-fonts
    noto-fonts-emoji
    papirus-icon-theme
    breeze
    breeze-gtk
)

sudo pacman -S --needed --noconfirm "${THEME_PACKAGES[@]}" || log_warning "Some font/theme packages may not be available"

# Install SDDM
log "Setting up SDDM..."
if command -v sddm &> /dev/null; then
    sudo pacman -S --needed --noconfirm sddm-kcm || log_warning "SDDM config module not available"
    log_info "SDDM detected"
else
    log_warning "SDDM not installed - install with: sudo pacman -S sddm"
fi

# Create config directory structure
log "Setting up configuration directories..."
mkdir -p "$CONFIG_DIR/hypr"
mkdir -p "$HOME/Pictures/Screenshots"

# Link dotfiles from ~/dotfiles/.config/
log "Linking configuration files from $DOTFILES_DIR..."

# Link hyprland config if exists in dotfiles
if [[ -f "$DOTFILES_DIR/hypr/hyprland.conf" ]]; then
    ln -sf "$DOTFILES_DIR/hypr/hyprland.conf" "$CONFIG_DIR/hypr/hyprland.conf"
    log_info "Linked hyprland.conf from dotfiles"
else
    log_warning "hyprland.conf not found in $DOTFILES_DIR/hypr/"
    log_warning "Please create it there, then run: ln -sf $DOTFILES_DIR/hypr/hyprland.conf $CONFIG_DIR/hypr/hyprland.conf"
fi

# Link hyprlock config if exists
if [[ -f "$DOTFILES_DIR/hypr/hyprlock.conf" ]]; then
    ln -sf "$DOTFILES_DIR/hypr/hyprlock.conf" "$CONFIG_DIR/hypr/hyprlock.conf"
    log_info "Linked hyprlock.conf from dotfiles"
else
    log_warning "hyprlock.conf not found in $DOTFILES_DIR/hypr/"
fi

# Link hypridle config if exists
if [[ -f "$DOTFILES_DIR/hypr/hypridle.conf" ]]; then
    ln -sf "$DOTFILES_DIR/hypr/hypridle.conf" "$CONFIG_DIR/hypr/hypridle.conf"
    log_info "Linked hypridle.conf from dotfiles"
else
    log_warning "hypridle.conf not found in $DOTFILES_DIR/hypr/"
fi

# Link waybar config if exists
if [[ -d "$DOTFILES_DIR/waybar" ]]; then
    ln -sf "$DOTFILES_DIR/waybar" "$CONFIG_DIR/waybar"
    log_info "Linked waybar config from dotfiles"
else
    log_warning "waybar config not found in $DOTFILES_DIR/waybar/"
fi

# Link rofi config if exists
if [[ -d "$DOTFILES_DIR/rofi" ]]; then
    ln -sf "$DOTFILES_DIR/rofi" "$CONFIG_DIR/rofi"
    log_info "Linked rofi config from dotfiles"
else
    log_warning "rofi config not found in $DOTFILES_DIR/rofi/"
fi

# Enable essential services
log "Enabling system services..."
sudo systemctl enable --now bluetooth.service || log_warning "Failed to enable bluetooth"
sudo systemctl enable --now NetworkManager.service || log_warning "Failed to enable NetworkManager"

# Enable SDDM
if command -v sddm &> /dev/null; then
    sudo systemctl enable sddm.service || log_warning "Failed to enable SDDM"
    log_info "SDDM enabled - will start on next boot"
fi

# Enable pipewire
systemctl --user enable --now pipewire.service || log_warning "Failed to enable pipewire"
systemctl --user enable --now wireplumber.service || log_warning "Failed to enable wireplumber"

# Create Hyprland desktop entry for SDDM
log "Creating Hyprland desktop entry..."
sudo mkdir -p /usr/share/wayland-sessions
sudo tee /usr/share/wayland-sessions/hyprland.desktop > /dev/null << EOF
[Desktop Entry]
Name=Hyprland
Comment=An intelligent dynamic tiling Wayland compositor
Exec=Hyprland
Type=Application
EOF

# Create environment variables
log "Setting up environment variables..."
mkdir -p "$HOME/.config/environment.d"
cat > "$HOME/.config/environment.d/hyprland.conf" << EOF
XCURSOR_SIZE=24
QT_QPA_PLATFORMTHEME=qt5ct
QT_QPA_PLATFORM=wayland
QT_WAYLAND_DISABLE_WINDOWDECORATION=1
GDK_BACKEND=wayland,x11
SDL_VIDEODRIVER=wayland
CLUTTER_BACKEND=wayland
XDG_CURRENT_DESKTOP=Hyprland
XDG_SESSION_TYPE=wayland
XDG_SESSION_DESKTOP=Hyprland
MOZ_ENABLE_WAYLAND=1
EOF

# Check for AUR helper
log "Checking for AUR helper..."
if command -v yay &> /dev/null; then
    log_info "Found yay. Install missing packages with:"
    echo "  yay -S swww ghostty google-chrome"
elif command -v paru &> /dev/null; then
    log_info "Found paru. Install missing packages with:"
    echo "  paru -S swww ghostty google-chrome"
else
    log_warning "No AUR helper found. Install 'yay' first:"
    echo "  git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si"
fi

# Verify installation
if ! command -v Hyprland &> /dev/null; then
    error_exit "Hyprland installation verification failed"
fi

# Print summary
echo ""
log "${GREEN}═══════════════════════════════════════════════════════${NC}"
log "${GREEN}Installation completed successfully!${NC}"
log "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo ""
log_info "Configuration linked from: $DOTFILES_DIR"
log_info "Active config location: $CONFIG_DIR/hypr/"
log_info "Log file: $LOG_FILE"
echo ""
log_info "${YELLOW}Next steps:${NC}"
echo "  1. Add your config files to: $DOTFILES_DIR/hyprland/"
echo "  2. Install AUR packages: yay -S swww ghostty google-chrome"
echo "  3. Add wallpaper: ~/Pictures/wallpaper.jpg"
echo "  4. Reboot or run: sudo systemctl start sddm"
echo "  5. Select 'Hyprland' from SDDM"
echo "  6. Run 'hyprctl monitors' to verify monitor names"
echo ""
log_info "${YELLOW}Config files needed in $DOTFILES_DIR/hyprland/:${NC}"
echo "  - hyprland.conf (main config)"
echo "  - hyprlock.conf (lock screen)"
echo "  - hypridle.conf (idle management)"
echo ""
log "${GREEN}Setup complete! Check the separate config artifacts.${NC}"
