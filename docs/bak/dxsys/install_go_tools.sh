#!/bin/bash

set -uo pipefail

# Function to install a Go tool
install_tool() {
    local name=$1
    local repo=$2
    echo "Installing $name..."
    if go install "$repo"; then
        echo "$name installed successfully"
    else
        echo "Failed to install $name"
    fi
}

# Install Go tools
install_tool "gocode" "github.com/nsf/gocode@latest"
install_tool "gopkgs" "github.com/tpng/gopkgs@latest"
install_tool "go-outline" "github.com/ramya-rao-a/go-outline@latest"
install_tool "go-symbols" "github.com/acroca/go-symbols@latest"
install_tool "guru" "golang.org/x/tools/cmd/guru@latest"
install_tool "gorename" "golang.org/x/tools/cmd/gorename@latest"
install_tool "gomodifytags" "github.com/fatih/gomodifytags@latest"
install_tool "impl" "github.com/josharian/impl@latest"
install_tool "godoc" "golang.org/x/tools/cmd/godoc@latest"
install_tool "goimports" "golang.org/x/tools/cmd/goimports@latest"
install_tool "dlv" "github.com/go-delve/delve/cmd/dlv@latest"

echo "Go tools installation completed"
echo "Successfully installed tools should be available in your Go bin directory"
