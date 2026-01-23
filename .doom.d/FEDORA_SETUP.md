# Fedora Setup Guide for Doom Emacs

Development tools and LSP servers for Python/Flask, Go, C++, and web development.

## Core Tools

```bash
sudo dnf install -y git ripgrep fd-find cmake libtool libvterm
```

## Python (Flask Development)

```bash
# Python + Pyright LSP
sudo dnf install -y python3-pip python3-virtualenv
npm install -g pyright

# Formatters
pip install --user black isort ruff

# Per-project: create .venv
python -m venv .venv
source .venv/bin/activate
pip install flask  # or your requirements
```

**Pyright config** (optional `pyrightconfig.json` in project root):
```json
{
  "venvPath": ".",
  "venv": ".venv"
}
```

## Go

```bash
sudo dnf install -y golang
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gofmt@latest
```

## C/C++

```bash
sudo dnf install -y clang clang-tools-extra cmake
```

For compile_commands.json:
```bash
# CMake projects
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .

# Or install Bear for Makefile projects
sudo dnf install -y bear
bear -- make
```

## Rust

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rust-analyzer rustfmt clippy
```

## Web / JavaScript / TypeScript

```bash
sudo dnf install -y nodejs npm

npm install -g typescript typescript-language-server
npm install -g vscode-langservers-extracted  # HTML/CSS/JSON
npm install -g prettier
```

## Shell

```bash
sudo dnf install -y ShellCheck shfmt
```

## YAML / Docker / Terraform

```bash
# YAML
npm install -g yaml-language-server

# Docker
npm install -g dockerfile-language-server-nodejs

# Terraform
sudo dnf config-manager --add-repo https://rpm.releases.hashicorp.com/fedora/hashicorp.repo
sudo dnf install -y terraform
# Get terraform-ls from: https://github.com/hashicorp/terraform-ls/releases
```

## Doom Emacs Setup

```bash
# Install Doom
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# After config changes
doom sync
```

## Verify Installation

```bash
# LSP servers
which pyright gopls clangd rust-analyzer

# Formatters
which black goimports rustfmt prettier shfmt
```

## Python Project Workflow

```bash
# 1. Create project with venv
mkdir myproject && cd myproject
python -m venv .venv

# 2. Activate (Emacs does this automatically)
source .venv/bin/activate

# 3. Install deps
pip install flask black isort

# 4. Open in Emacs - LSP auto-starts with correct venv
```
