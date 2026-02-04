# xynapz NixOS Dotfiles

Declarative NixOS configuration using flakes and Home Manager.

## Quick Start

```bash
# Clone repository
git clone https://github.com/xynapz/dotfiles ~/dotfiles
cd ~/dotfiles

# Build and activate
sudo nixos-rebuild switch --flake .#xynapz
```

See [MANUAL_SETUP.org](MANUAL_SETUP.org) for required post-install steps.

## System Components

### Window Manager
- Sway (Wayland compositor)
- Waybar (status bar)
- Fuzzel (app launcher)
- Swaylock (screen locker)
- Mako (notifications)

### Applications
- WezTerm (terminal)
- Doom Emacs (editor)
- Firefox (browser)
- Nautilus (file manager)

### Development
- Git with delta diff viewer
- GitHub CLI
- Language servers: pyright, gopls, typescript, bash, lua, terraform, yaml
- Formatters: prettier, black, ruff, stylua, shfmt, nixpkgs-fmt
- Rust toolchain via rustup
- Node.js, Python 3, Go

## Keybindings

| Key | Action |
|-----|--------|
| `Mod+Return` | Terminal |
| `Mod+Shift+Return` | Emacs |
| `Mod+Shift+b` | Firefox |
| `Mod+f` | File manager |
| `Mod+Space` | App launcher |
| `Mod+v` | Clipboard history |
| `Mod+Shift+w` | Wallpaper selector |
| `Mod+Shift+p` | Power menu |
| `Mod+Alt+l` | Lock screen |
| `Mod+p` | Color picker |
| `Print` | Screenshot (area) |
| `Shift+Print` | Screenshot (full) |
| `Mod+Print` | Screenshot (save) |

### Window Management
| Key | Action |
|-----|--------|
| `Mod+h/j/k/l` | Focus window |
| `Mod+Shift+h/j/k/l` | Move window |
| `Mod+1-9/0` | Switch workspace |
| `Mod+Shift+1-9/0` | Move to workspace |
| `Mod+Control+h/l` | Move workspace to output |
| `Mod+Shift+q` | Close window |
| `Mod+Shift+f` | Fullscreen |
| `Mod+Shift+Space` | Toggle floating |
| `Mod+r` | Resize mode |

## Commands

### System Management
```bash
# Rebuild system
sudo nixos-rebuild switch --flake ~/dotfiles#xynapz

# Update flake inputs
nix flake update

# Garbage collection
sudo nix-collect-garbage -d

# Rollback
sudo nixos-rebuild switch --rollback
```

### Doom Emacs
```bash
# Sync after config changes
doom sync

# Upgrade Doom
doom upgrade

# Check for issues
doom doctor
```

### Waydroid
```bash
# Launch Android UI
waydroid show-full-ui

# Install APK
waydroid app install app.apk

# List apps
waydroid app list
```

## Configuration Files

Configuration files in `config/` are linked to their proper locations via Home
Manager. To modify:

1. Edit files in `config/`
2. Rebuild: `sudo nixos-rebuild switch --flake ~/dotfiles#xynapz`

## Manual Setup Required

Some components require manual setup that cannot be fully declarative:

- Doom Emacs installation
- Wallpaper directory setup
- SSH key generation
- Bluetooth pairing
- Waydroid initialization

See [MANUAL_SETUP.org](MANUAL_SETUP.org) for detailed instructions.

## Hardware

- CPU: AMD (kvm-amd)
- Displays: HDMI-A-1 (2560x1440@99.9Hz), eDP-1 (1920x1080)
- Workspaces 1-9 on external monitor, workspace 10 on laptop screen

## License

Personal configuration files. Use as reference or fork as needed.
