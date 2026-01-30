# xynapz NixOS Dotfiles

Declarative NixOS configuration with Home Manager and Flakes.

## Quick Start

### Fresh NixOS Install

```bash
# 1. Boot NixOS installer and partition disks

# 2. Generate hardware config
sudo nixos-generate-config --root /mnt
# Copy the hardware config
cp /mnt/etc/nixos/hardware-configuration.nix hosts/xynapz/

# 3. Clone dotfiles
git clone https://github.com/xynapz/dotfiles ~/dotfiles
cd ~/dotfiles

# 4. Build and switch
sudo nixos-rebuild switch --flake .#xynapz

# 5. Install Doom Emacs (first time only)
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

### Daily Usage

```bash
# Rebuild after config changes
sudo nixos-rebuild switch --flake ~/dotfiles#xynapz

# Update all packages (flake inputs)
nix flake update
sudo nixos-rebuild switch --flake .#xynapz

# Rollback if something breaks
sudo nixos-rebuild switch --rollback

# List generations
sudo nix-env --list-generations --profile /nix/var/nix/profiles/system

# Garbage collect old generations
sudo nix-collect-garbage -d
```

## Structure

```
dotfiles/
├── flake.nix                 # Flake entry point
├── flake.lock                # Locked dependencies
├── hosts/
│   └── xynapz/
│       ├── configuration.nix # System config
│       └── hardware-configuration.nix
├── home/
│   ├── home.nix              # Home Manager entry
│   └── modules/              # Per-app configs
│       ├── shell.nix         # Bash + starship
│       ├── sway.nix          # Window manager
│       ├── bar.nix           # Waybar
│       ├── launcher.nix      # Fuzzel
│       ├── notifications.nix # Mako
│       ├── screenshots.nix   # Swappy
│       ├── lockscreen.nix    # Swaylock
│       ├── doom-emacs.nix    # Emacs + Doom
│       ├── wezterm.nix       # Terminal
│       └── git.nix           # Git config
├── config/                   # Raw config files
│   ├── doom/                 # Doom Emacs config
│   ├── wezterm/              # WezTerm Lua config
│   └── swappy/               # Screenshot config
└── scripts/                  # Helper scripts
    ├── power-menu.sh
    ├── clipboard-menu.sh
    └── color-picker.sh
```

## Keybindings

| Key | Action |
|-----|--------|
| `Mod+Return` | Terminal (wezterm) |
| `Mod+Shift+Return` | Emacs client |
| `Mod+Space` | App launcher (fuzzel) |
| `Mod+v` | Clipboard history |
| `Mod+Shift+p` | Power menu |
| `Mod+Alt+l` | Lock screen |
| `Print` | Screenshot region → edit |
| `Shift+Print` | Screenshot full → edit |
| `Mod+Print` | Screenshot region → file |

## DMS Replacement

| DMS Feature | Replacement |
|-------------|-------------|
| Spotlight | Fuzzel |
| Bar | Waybar |
| Notifications | Mako |
| Lock screen | Swaylock |
| Clipboard | cliphist + fuzzel script |
| Screenshots | grim + slurp + swappy |
| Power menu | fuzzel script |

## VM Testing

1. Download NixOS minimal ISO
2. Create VM (4GB RAM, 30GB disk, 3D acceleration)
3. Boot and run the Quick Start steps
4. Verify all keybindings work

## Customization

Edit files in `home/modules/` and rebuild:
```bash
sudo nixos-rebuild switch --flake ~/dotfiles#xynapz
```

For Doom Emacs changes:
```bash
doom sync
```
