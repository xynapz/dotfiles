#!/bin/bash

set -euo pipefail

# Function to log messages
log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $1"
}

# Function to handle errors
handle_error() {
    log "Error occurred on line $1"
    exit 1
}

# Set up error handling
trap 'handle_error $LINENO' ERR

# Backup function
backup_ssh_config() {
    if [[ -f ~/.ssh/config ]]; then
        backup_file="$HOME/.ssh/config.backup_$(date +'%Y%m%d%H%M%S')"
        cp ~/.ssh/config "$backup_file"
        log "Existing SSH config backed up to $backup_file"
    fi
}

# Check and install required packages
install_packages() {
    log "Checking and installing required packages..."
    sudo apt update
    sudo apt install -y openssh-client git
}

# Create SSH directories
create_ssh_directories() {
    log "Creating SSH directories..."
    mkdir -p ~/.ssh/github
    chmod 700 ~/.ssh ~/.ssh/github
}

# Generate SSH key
generate_ssh_key() {
    log "Generating SSH key..."
    read -p "Enter your email address for the SSH key: " email
    ssh-keygen -t rsa -b 4096 -C "$email" -f ~/.ssh/github/id_rsa -q -N ''
}

# Update SSH config
update_ssh_config() {
    log "Updating SSH config..."
    if [[ -f ~/.ssh/config ]]; then
        if grep -q "Host github.com" ~/.ssh/config; then
            log "GitHub configuration already exists in SSH config. Skipping..."
            return
        fi
    fi
    
    echo -e "\nHost github.com\n    IdentityFile ~/.ssh/github/id_rsa" >> ~/.ssh/config
    chmod 600 ~/.ssh/config
}

# Main function
main() {
    log "Starting Git SSH setup process..."
    
    install_packages
    create_ssh_directories
    backup_ssh_config
    generate_ssh_key
    update_ssh_config
    
    log "Setup complete!"
    log "Here's your public SSH key. Please add this to your GitHub account:"
    echo "-------------------"
    cat ~/.ssh/github/id_rsa.pub
    echo "-------------------"
    log "After adding the key to GitHub, test your setup with: ssh -T git@github.com"
}

# Run the main function
main
