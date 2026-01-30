# xynapz NixOS Dotfiles

Declarative NixOS + Home Manager + Flakes configuration.

## Quick Start

```bash
# On NixOS:
cd ~/dotfiles
sudo nixos-rebuild switch --flake .#xynapz

# First time Doom Emacs:
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

## Structure

```
dotfiles/
├── flake.nix                   # Entry point
├── hosts/xynapz/
│   ├── configuration.nix       # System config
│   └── hardware-configuration.nix  # Regenerate per machine
├── home/
│   ├── home.nix                # User config
│   └── modules/*.nix           # App configs
├── scripts/                    # Helper scripts
└── .config/                    # Raw config files
    ├── doom/                   # Doom Emacs
    ├── wezterm/                # WezTerm
    └── swappy/                 # Screenshots
```

## Keybindings

| Key | Action |
|-----|--------|
| `Mod+Return` | Terminal |
| `Mod+Space` | App launcher |
| `Mod+v` | Clipboard |
| `Mod+Shift+p` | Power menu |
| `Mod+Alt+l` | Lock |
| `Print` | Screenshot |

## Commands

```bash
# Rebuild
sudo nixos-rebuild switch --flake ~/dotfiles#xynapz

# Update
nix flake update && sudo nixos-rebuild switch --flake .#xynapz

# Rollback
sudo nixos-rebuild switch --rollback
```
