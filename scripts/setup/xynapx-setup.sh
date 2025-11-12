#!/bin/bash

#====================================================================================================
# Arch Linux Post-Installation Setup Script After TTY User Login
# Description: Automates system setup after minimal Arch installation through archinstall script
# Author: angel
# Usage: ./setup.sh [--skip-system-update] [--skip-drivers]
# Entry: main()
#====================================================================================================

set -euo pipefail  # exit on error, undefined vars, pipe failures
IFS=$'\n\t'        # internal field separator to newline and tab

#==============================================================================
# Configuration
#==============================================================================

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly LOG_FILE="${HOME}/arch-setup-$(date +%Y%m%d-%H%M%S).log"
readonly DOTFILES_REPO="https://github.com/angel-dx/dotfiles.git"
readonly DOTFILES_DIR="${HOME}/.dotfiles"
readonly YAY_REPO="https://aur.archlinux.org/yay-bin.git"
readonly YAY_BUILD_DIR="/tmp/yay-build"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

#==============================================================================
# Logging Functions
#==============================================================================

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp
    timestamp="$(date '+%Y-%m-%d %H:%M:%S')"
    echo "[${timestamp}] [${level}] ${message}" | tee -a "${LOG_FILE}"
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" | tee -a "${LOG_FILE}"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" | tee -a "${LOG_FILE}"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*" | tee -a "${LOG_FILE}"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" | tee -a "${LOG_FILE}"
}

#==============================================================================
# Error Handling
#==============================================================================

cleanup() {
    log_info "Cleaning up temporary files..."
    rm -rf "${YAY_BUILD_DIR}" 2>/dev/null || true
}

error_exit() {
    log_error "$1"
    cleanup
    exit 1
}

trap cleanup EXIT
trap 'error_exit "Script interrupted"' INT TERM

#==============================================================================
# Validation Functions
#==============================================================================

check_root() {
    if [[ $EUID -eq 0 ]]; then
        error_exit "This script should NOT be run as root. Run as your regular user."
    fi
}

check_internet() {
    log_info "Checking internet connectivity..."
    if ! ping -c 1 archlinux.org &>/dev/null; then
        error_exit "No internet connection. Please check your network."
    fi
    log_success "Internet connection verified"
}

check_arch() {
    if [[ ! -f /etc/arch-release ]]; then
        error_exit "This script is designed for Arch Linux only"
    fi
}

#==============================================================================
# Package Management Functions
#==============================================================================

update_system() {
    log_info "Updating system packages..."
    sudo pacman -Syu --noconfirm || error_exit "System update failed"
    log_success "System updated successfully"
}

install_pacman_packages() {
    local packages=("$@")
    log_info "Installing packages: ${packages[*]}"

    for pkg in "${packages[@]}"; do
        if pacman -Qi "$pkg" &>/dev/null; then
            log_info "Package '$pkg' already installed, skipping"
        else
            sudo pacman -S --noconfirm "$pkg" || log_warning "Failed to install $pkg"
        fi
    done

    log_success "Package installation completed"
}

#==============================================================================
# Hardware Drivers Installation
#==============================================================================

install_gpu_drivers() {
    log_info "Detecting GPU and installing drivers..."

    local gpu_vendor
    gpu_vendor=$(lspci | grep -E "VGA|3D" | head -n 1)

    if echo "$gpu_vendor" | grep -i nvidia &>/dev/null; then
        log_info "NVIDIA GPU detected"
        install_pacman_packages nvidia nvidia-utils nvidia-settings
    elif echo "$gpu_vendor" | grep -i amd &>/dev/null; then
        log_info "AMD GPU detected"
        install_pacman_packages mesa vulkan-radeon libva-mesa-driver mesa-vdpau
    elif echo "$gpu_vendor" | grep -i intel &>/dev/null; then
        log_info "Intel GPU detected"
        install_pacman_packages mesa vulkan-intel libva-intel-driver intel-media-driver
    else
        log_warning "Could not detect GPU vendor, installing generic drivers"
        install_pacman_packages mesa
    fi

    log_success "GPU drivers installed"
}

install_hardware_drivers() {
    log_info "Installing common hardware drivers and firmware..."

    local driver_packages=(
        linux-firmware
        linux-headers
        base-devel
        sof-firmware        # Sound Open Firmware
        alsa-utils          # Audio utilities
        pulseaudio          # Audio server (or pipewire-pulse)
        pulseaudio-alsa
        bluez               # Bluetooth support
        bluez-utils
    )

    install_pacman_packages "${driver_packages[@]}"
    install_gpu_drivers

    log_success "Hardware drivers installed"
}

#==============================================================================
# YAY Installation
#==============================================================================

install_yay() {
    if command -v yay &>/dev/null; then
        log_info "yay is already installed"
        return 0
    fi

    log_info "Installing yay AUR helper..."

    # Ensure git is installed
    sudo pacman -S --needed --noconfirm git base-devel

    # Clone and build yay
    rm -rf "${YAY_BUILD_DIR}"
    git clone "${YAY_REPO}" "${YAY_BUILD_DIR}" || error_exit "Failed to clone yay repository"

    cd "${YAY_BUILD_DIR}"
    makepkg -si --noconfirm || error_exit "Failed to build yay"
    cd - &>/dev/null

    log_success "yay installed successfully"
}

#==============================================================================
# Development Tools Installation
#==============================================================================

install_dev_tools() {
    log_info "Installing development tools..."

    local dev_packages=(
        # Essential
        git
        curl
        wget
        vim
        gcc
        make
        cmake

        # Programming Languages & Tools
        nodejs
        npm
        python
        python-pip
        go
        rust

        # Build Tools
        base-devel
        autoconf
        automake
        pkgconf

        # Version Control & Tools
        github-cli

        # Optional but Useful
        jq              # JSON processor
        ripgrep         # Fast grep alternative
        fd              # Fast find alternative
        bat             # Cat with syntax highlighting
        fzf             # Fuzzy finder
        htop            # Process viewer
        ncdu            # Disk usage analyzer
        tree            # Directory tree viewer
        unzip
        zip
        tar
        stow            # Symlink manager for dotfiles

        # Network Tools
        openssh
        rsync
        nmap
    )

    install_pacman_packages "${dev_packages[@]}"
    log_success "Development tools installed"
}

#==============================================================================
# Hyprland Dependencies Installation
#==============================================================================

install_hyprland_deps() {
    log_info "Installing Hyprland dependencies..."

    local hyprland_packages=(
        # Display Server
        wayland
        wayland-protocols

        # Hyprland Core Dependencies
        hyprland
        xdg-desktop-portal-hyprland

        # Window Management
        polkit-kde-agent
        qt5-wayland
        qt6-wayland

        # Essential Wayland Tools
        wl-clipboard       # Clipboard manager
        wlroots

        # Notifications
        dunst

        # Application Launcher
        rofi-wayland

        # Status Bar (if not using waybar from your script)
        waybar

        # Screenshot & Screen Recording
        grim               # Screenshot tool
        slurp              # Screen area selector

        # File Manager
        thunar

        # Media
        imv                # Image viewer
        mpv                # Video player

        # PDF Reader
        zathura
        zathura-pdf-mupdf

        # Fonts
        ttf-font-awesome
        ttf-dejavu
        noto-fonts
        noto-fonts-emoji
    )

    install_pacman_packages "${hyprland_packages[@]}"
    log_success "Hyprland dependencies installed"
}

#==============================================================================
# Shell Configuration
#==============================================================================

install_zsh() {
    log_info "Installing and configuring Zsh..."

    install_pacman_packages zsh

    # Install Oh My Zsh
    if [[ ! -d "${HOME}/.oh-my-zsh" ]]; then
        log_info "Installing Oh My Zsh..."
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended || \
            error_exit "Failed to install Oh My Zsh"
        log_success "Oh My Zsh installed"
    else
        log_info "Oh My Zsh already installed"
    fi

    # Change default shell to zsh
    if [[ "$SHELL" != "$(which zsh)" ]]; then
        log_info "Changing default shell to zsh..."
        chsh -s "$(which zsh)" || log_warning "Failed to change shell. Run 'chsh -s \$(which zsh)' manually"
    fi

    log_success "Zsh configured successfully"
}

#==============================================================================
# Dotfiles Management
#==============================================================================

clone_dotfiles() {
    log_info "Cloning dotfiles repository..."

    if [[ -d "${DOTFILES_DIR}" ]]; then
        log_warning "Dotfiles directory already exists. Pulling latest changes..."
        cd "${DOTFILES_DIR}"
        git pull || log_warning "Failed to update dotfiles"
        cd - &>/dev/null
    else
        git clone "${DOTFILES_REPO}" "${DOTFILES_DIR}" || error_exit "Failed to clone dotfiles"
        log_success "Dotfiles cloned successfully"
    fi
}

link_config_file() {
    local source="$1"
    local target="$2"

    if [[ ! -e "${source}" ]]; then
        log_warning "Source file does not exist: ${source}"
        return 1
    fi

    # Backup existing file if it exists and is not a symlink
    if [[ -e "${target}" && ! -L "${target}" ]]; then
        log_info "Backing up existing file: ${target}"
        mv "${target}" "${target}.backup.$(date +%Y%m%d-%H%M%S)"
    fi

    # Remove existing symlink if present
    [[ -L "${target}" ]] && rm "${target}"

    # Create parent directory if needed
    mkdir -p "$(dirname "${target}")"

    # Create symlink
    ln -sf "${source}" "${target}" || {
        log_error "Failed to link ${source} to ${target}"
        return 1
    }

    log_success "Linked: ${target} -> ${source}"
}

setup_dotfiles() {
    log_info "Setting up dotfiles..."

    # Link .zshrc
    link_config_file "${DOTFILES_DIR}/.zshrc" "${HOME}/.zshrc"

    # Link .tmux.conf
    link_config_file "${DOTFILES_DIR}/.tmux.conf" "${HOME}/.tmux.conf"

    # Link Neovim config
    if [[ -d "${DOTFILES_DIR}/.config/nvim" ]]; then
        link_config_file "${DOTFILES_DIR}/.config/nvim" "${HOME}/.config/nvim"
    fi

    # Link Ghostty config
    if [[ -d "${DOTFILES_DIR}/.config/ghostty" ]]; then
        link_config_file "${DOTFILES_DIR}/.config/ghostty" "${HOME}/.config/ghostty"
    fi

    log_success "Dotfiles configured successfully"
}

#==============================================================================
# Terminal Tools Installation
#==============================================================================

install_tmux() {
    log_info "Installing tmux..."
    install_pacman_packages tmux

    # Install TPM (Tmux Plugin Manager) if not present
    local tpm_dir="${HOME}/.tmux/plugins/tpm"
    if [[ ! -d "${tpm_dir}" ]]; then
        log_info "Installing Tmux Plugin Manager..."
        git clone https://github.com/tmux-plugins/tpm "${tpm_dir}" || \
            log_warning "Failed to install TPM"
    fi

    log_success "tmux installed"
}

install_neovim() {
    log_info "Installing Neovim..."
    install_pacman_packages neovim

    # Install common Neovim dependencies
    local nvim_deps=(
        python-pynvim
        xclip           # Clipboard support
        wl-clipboard    # Wayland clipboard
    )
    install_pacman_packages "${nvim_deps[@]}"

    log_success "Neovim installed"
}

install_ghostty() {
    log_info "Installing Ghostty terminal..."

    # Ghostty is typically in AUR
    if command -v yay &>/dev/null; then
        yay -S --noconfirm ghostty-bin || yay -S --noconfirm ghostty || \
            log_warning "Failed to install Ghostty. You may need to install it manually."
    else
        log_warning "yay not available. Skipping Ghostty installation."
    fi

    log_success "Ghostty installation attempted"
}

#==============================================================================
# Main Installation Flow
#==============================================================================

print_banner() {
    cat << "EOF"
╔═══════════════════════════════════════════════════════════╗
║                                                           ║
║       Arch Linux Post-Installation Setup Script          ║
║                                                           ║
╚═══════════════════════════════════════════════════════════╝
EOF
}

parse_arguments() {
    SKIP_SYSTEM_UPDATE=false
    SKIP_DRIVERS=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            --skip-system-update)
                SKIP_SYSTEM_UPDATE=true
                shift
                ;;
            --skip-drivers)
                SKIP_DRIVERS=true
                shift
                ;;
            -h|--help)
                echo "Usage: $0 [OPTIONS]"
                echo "Options:"
                echo "  --skip-system-update    Skip system update step"
                echo "  --skip-drivers          Skip hardware driver installation"
                echo "  -h, --help              Show this help message"
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

main() {
    parse_arguments "$@"

    print_banner
    log_info "Starting Arch Linux setup script"
    log_info "Log file: ${LOG_FILE}"

    # Pre-flight checks
    check_root
    check_arch
    check_internet

    # System update
    if [[ "${SKIP_SYSTEM_UPDATE}" == false ]]; then
        update_system
    else
        log_info "Skipping system update (--skip-system-update flag)"
    fi

    # Hardware drivers
    if [[ "${SKIP_DRIVERS}" == false ]]; then
        install_hardware_drivers
    else
        log_info "Skipping driver installation (--skip-drivers flag)"
    fi

    # Install yay
    install_yay

    # Development tools
    install_dev_tools

    # Hyprland dependencies
    install_hyprland_deps

    # Shell setup
    install_zsh

    # Dotfiles
    clone_dotfiles

    # Terminal tools
    install_tmux
    install_neovim
    install_ghostty

    # Configure dotfiles
    setup_dotfiles

    # Cleanup
    cleanup

    # Final message
    echo ""
    log_success "=========================================="
    log_success "Setup completed successfully!"
    log_success "=========================================="
    echo ""
    log_info "Next steps:"
    echo "  1. Log out and log back in (or run 'exec zsh') to activate Zsh"
    echo "  2. Run your Hyprland setup script"
    echo "  3. Open tmux and press prefix + I to install tmux plugins"
    echo "  4. Open Neovim to let it install plugins automatically"
    echo ""
    log_info "Log file saved to: ${LOG_FILE}"
}

#==============================================================================
# Script Entry Point
#==============================================================================

main "$@"
