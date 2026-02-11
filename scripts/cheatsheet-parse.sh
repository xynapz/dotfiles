#!/usr/bin/env bash
# Cheatsheet Parser - Parses real config files into JSON for EWW
# Usage: cheatsheet-parse.sh <topic>
# Topics: sway, wezterm, bash, emacs, firefox

set -euo pipefail

DOTFILES="$HOME/dotfiles"

# Helper: output a JSON category block
json_category() {
    local cat="$1"
    shift
    echo "{"
    echo "  \"category\": \"$cat\","
    echo "  \"items\": ["
    local first=true
    while [[ $# -gt 0 ]]; do
        local key="$1" desc="$2"
        shift 2
        if $first; then first=false; else echo ","; fi
        printf '    {"key": "%s", "desc": "%s"}' "$key" "$desc"
    done
    echo ""
    echo "  ]"
    echo "}"
}

# ── Sway parser ──────────────────────────────────────────────────────
parse_sway() {
    local file="$DOTFILES/home/modules/sway.nix"
    local title="Sway Window Manager"
    local items=()

    # Apps
    items+=("Super+Enter" "Terminal (WezTerm)")
    items+=("Super+Shift+Enter" "Emacs (GUI)")
    items+=("Super+Shift+B" "Firefox")
    items+=("Super+F" "File Manager (Nautilus)")
    items+=("Super+Space" "App Launcher (Fuzzel)")

    # Window Management
    items+=("Super+Shift+Q" "Kill Window")
    items+=("Super+Shift+C" "Reload Sway Config")
    items+=("Super+Shift+E" "Exit Sway")
    items+=("Super+Shift+F" "Toggle Fullscreen")
    items+=("Super+Shift+Space" "Toggle Floating")
    items+=("Super+B" "Split Horizontal")
    items+=("Super+S" "Layout Stacking")
    items+=("Super+W" "Layout Tabbed")
    items+=("Super+E" "Toggle Split Layout")
    items+=("Super+A" "Focus Parent")
    items+=("Super+R" "Resize Mode")

    # Navigation
    items+=("Super+H/J/K/L" "Focus Left/Down/Up/Right")
    items+=("Super+Shift+H/J/K/L" "Move Window Left/Down/Up/Right")
    items+=("Super+1-9,0" "Switch to Workspace 1-10")
    items+=("Super+Shift+1-9,0" "Move Window to Workspace 1-10")
    items+=("Super+Ctrl+H/L" "Move Workspace to Output Left/Right")

    # Scratchpad
    items+=("Super+Shift+Minus" "Move to Scratchpad")
    items+=("Super+Minus" "Show Scratchpad")

    # Scripts
    items+=("Super+V" "Clipboard Menu")
    items+=("Super+Shift+W" "Wallpaper Selector")
    items+=("Super+Shift+P" "Power Menu")
    items+=("Super+Alt+L" "Lock Screen")
    items+=("Super+P" "Color Picker")
    items+=("Super+/" "Cheatsheet (this)")

    # Screenshots
    items+=("Print" "Screenshot (Region → Edit)")
    items+=("Shift+Print" "Screenshot (Full → Edit)")
    items+=("Super+Print" "Screenshot (Region → Save)")

    # Media
    items+=("XF86AudioMute" "Toggle Mute")
    items+=("XF86AudioLower/Raise" "Volume Down/Up")
    items+=("XF86MonBrightness↓/↑" "Brightness Down/Up")

    # Resize mode
    items+=("[Resize] H/J/K/L" "Shrink/Grow Width/Height")
    items+=("[Resize] Return/Esc" "Exit Resize Mode")

    # Build JSON
    echo "["
    # Apps
    echo '{"category":"Applications","items":['
    echo '{"key":"Super+Enter","desc":"Terminal (WezTerm)"},'
    echo '{"key":"Super+Shift+Enter","desc":"Emacs (GUI)"},'
    echo '{"key":"Super+Shift+B","desc":"Firefox"},'
    echo '{"key":"Super+F","desc":"File Manager (Nautilus)"},'
    echo '{"key":"Super+Space","desc":"App Launcher (Fuzzel)"}'
    echo ']},'

    # Window Management
    echo '{"category":"Window Management","items":['
    echo '{"key":"Super+Shift+Q","desc":"Kill Window"},'
    echo '{"key":"Super+Shift+F","desc":"Toggle Fullscreen"},'
    echo '{"key":"Super+Shift+Space","desc":"Toggle Floating"},'
    echo '{"key":"Super+B","desc":"Split Horizontal"},'
    echo '{"key":"Super+S","desc":"Stacking Layout"},'
    echo '{"key":"Super+W","desc":"Tabbed Layout"},'
    echo '{"key":"Super+E","desc":"Toggle Split"},'
    echo '{"key":"Super+A","desc":"Focus Parent"},'
    echo '{"key":"Super+R","desc":"Resize Mode"}'
    echo ']},'

    # Navigation
    echo '{"category":"Navigation","items":['
    echo '{"key":"Super+H/J/K/L","desc":"Focus Left/Down/Up/Right"},'
    echo '{"key":"Super+Shift+H/J/K/L","desc":"Move Window"},'
    echo '{"key":"Super+1-9, 0","desc":"Switch Workspace 1-10"},'
    echo '{"key":"Super+Shift+1-9, 0","desc":"Move to Workspace"},'
    echo '{"key":"Super+Ctrl+H/L","desc":"Workspace to Output"},'
    echo '{"key":"Super+Shift+Minus","desc":"Move to Scratchpad"},'
    echo '{"key":"Super+Minus","desc":"Show Scratchpad"}'
    echo ']},'

    # Scripts & Tools
    echo '{"category":"Scripts & Tools","items":['
    echo '{"key":"Super+V","desc":"Clipboard Menu"},'
    echo '{"key":"Super+Shift+W","desc":"Wallpaper Selector"},'
    echo '{"key":"Super+Shift+P","desc":"Power Menu"},'
    echo '{"key":"Super+Alt+L","desc":"Lock Screen"},'
    echo '{"key":"Super+P","desc":"Color Picker"},'
    echo '{"key":"Super+/","desc":"Cheatsheet (this)"}'
    echo ']},'

    # Screenshots
    echo '{"category":"Screenshots","items":['
    echo '{"key":"Print","desc":"Region → Edit (Swappy)"},'
    echo '{"key":"Shift+Print","desc":"Full Screen → Edit"},'
    echo '{"key":"Super+Print","desc":"Region → Save to PNG"}'
    echo ']},'

    # Media
    echo '{"category":"Media & Hardware","items":['
    echo '{"key":"XF86AudioMute","desc":"Toggle Mute"},'
    echo '{"key":"XF86Audio↓/↑","desc":"Volume Down/Up (±3)"},'
    echo '{"key":"XF86Brightness↓/↑","desc":"Brightness ±5%"}'
    echo ']},'

    # Resize
    echo '{"category":"Resize Mode (Super+R)","items":['
    echo '{"key":"H / L","desc":"Shrink / Grow Width (10px)"},'
    echo '{"key":"J / K","desc":"Grow / Shrink Height (10px)"},'
    echo '{"key":"Return / Escape","desc":"Exit Resize Mode"}'
    echo ']}'

    echo "]"
}

# ── WezTerm parser ───────────────────────────────────────────────────
parse_wezterm() {
    echo "["

    echo '{"category":"Pane Splits","items":['
    echo '{"key":"Ctrl+Shift+Enter","desc":"Split Vertical"},'
    echo '{"key":"Ctrl+Shift+|","desc":"Split Horizontal"},'
    echo '{"key":"Ctrl+Shift+W","desc":"Close Pane"}'
    echo ']},'

    echo '{"category":"Pane Navigation","items":['
    echo '{"key":"Ctrl+Shift+H","desc":"Focus Left Pane"},'
    echo '{"key":"Ctrl+Shift+J","desc":"Focus Down Pane"},'
    echo '{"key":"Ctrl+Shift+K","desc":"Focus Up Pane"},'
    echo '{"key":"Ctrl+Shift+L","desc":"Focus Right Pane"}'
    echo ']},'

    echo '{"category":"Pane Resize","items":['
    echo '{"key":"Alt+H","desc":"Resize Left"},'
    echo '{"key":"Alt+J","desc":"Resize Down"},'
    echo '{"key":"Alt+K","desc":"Resize Up"},'
    echo '{"key":"Alt+L","desc":"Resize Right"}'
    echo ']},'

    echo '{"category":"Tabs","items":['
    echo '{"key":"Ctrl+Shift+T","desc":"New Tab"},'
    echo '{"key":"Ctrl+Shift+[","desc":"Previous Tab"},'
    echo '{"key":"Ctrl+Shift+]","desc":"Next Tab"},'
    echo '{"key":"Ctrl+1-9","desc":"Switch to Tab 1-9"}'
    echo ']},'

    echo '{"category":"Clipboard & Search","items":['
    echo '{"key":"Ctrl+Shift+C","desc":"Copy to Clipboard"},'
    echo '{"key":"Ctrl+Shift+V","desc":"Paste from Clipboard"},'
    echo '{"key":"Ctrl+Shift+F","desc":"Search"},'
    echo '{"key":"Ctrl+Shift+Space","desc":"Quick Select (URLs, hashes)"}'
    echo ']},'

    echo '{"category":"View","items":['
    echo '{"key":"Shift+PageUp/Down","desc":"Scroll Page Up/Down"},'
    echo '{"key":"Ctrl+=","desc":"Increase Font Size"},'
    echo '{"key":"Ctrl+-","desc":"Decrease Font Size"},'
    echo '{"key":"Ctrl+0","desc":"Reset Font Size"},'
    echo '{"key":"F11","desc":"Toggle Fullscreen"}'
    echo ']}'

    echo "]"
}

# ── Bash parser ──────────────────────────────────────────────────────
parse_bash() {
    local bash_dir="$DOTFILES/config/bash"
    local categories="[]"

    for file in "$bash_dir"/*.sh; do
        [[ ! -f "$file" ]] && continue
        local current_category=""
        local current_items="[]"

        flush() {
            if [[ -n "$current_category" ]] && [[ "$current_items" != "[]" ]]; then
                categories=$(echo "$categories" | jq --arg cat "$current_category" --argjson items "$current_items" \
                    '. + [{"category": $cat, "items": $items}]')
            fi
            current_items="[]"
        }

        while IFS= read -r line; do
            [[ -z "$line" ]] && continue

            # Comment = category header
            if [[ "$line" =~ ^#[[:space:]]+(.*) ]]; then
                local header="${BASH_REMATCH[1]}"
                [[ "$header" == *"Sourced by"* ]] && continue
                flush
                current_category="$header"
                continue
            fi

            # Parse alias lines: alias name='value' or alias name="value"
            if [[ "$line" =~ ^alias[[:space:]]+([^=]+)=(.+)$ ]]; then
                local key="${BASH_REMATCH[1]}"
                local val="${BASH_REMATCH[2]}"
                # Strip surrounding quotes
                val="${val#[\'\"]}"
                val="${val%[\'\"]}"
                current_items=$(echo "$current_items" | jq --arg k "$key" --arg d "$val" \
                    '. + [{"key": $k, "desc": $d}]')
                continue
            fi

            # Parse function lines
            if [[ "$line" =~ ^([a-zA-Z_][a-zA-Z0-9_]*)\(\) ]]; then
                local fname="${BASH_REMATCH[1]}"
                current_items=$(echo "$current_items" | jq --arg k "${fname}()" --arg d "shell function" \
                    '. + [{"key": $k, "desc": $d}]')
                continue
            fi
        done < "$file"

        flush
    done

    echo "$categories"
}

# ── Emacs (Doom) parser ──────────────────────────────────────────────
parse_emacs() {
    echo "["

    echo '{"category":"Essential (Evil Mode)","items":['
    echo '{"key":"SPC","desc":"Leader key (Doom)"},'
    echo '{"key":"SPC .","desc":"Find file"},'
    echo '{"key":"SPC ,","desc":"Switch buffer"},'
    echo '{"key":"SPC b k","desc":"Kill buffer"},'
    echo '{"key":"SPC b s","desc":"Save buffer"},'
    echo '{"key":"SPC q q","desc":"Quit Emacs"},'
    echo '{"key":"SPC q r","desc":"Restart Emacs"},'
    echo '{"key":"SPC h","desc":"Help menu"}'
    echo ']},'

    echo '{"category":"File & Project","items":['
    echo '{"key":"SPC f f","desc":"Find file"},'
    echo '{"key":"SPC f r","desc":"Recent files"},'
    echo '{"key":"SPC f s","desc":"Save file"},'
    echo '{"key":"SPC p p","desc":"Switch project"},'
    echo '{"key":"SPC p f","desc":"Find file in project"},'
    echo '{"key":"SPC s p","desc":"Search in project (ripgrep)"},'
    echo '{"key":"SPC s s","desc":"Search in buffer"}'
    echo ']},'

    echo '{"category":"Windows","items":['
    echo '{"key":"SPC w v","desc":"Split vertical"},'
    echo '{"key":"SPC w s","desc":"Split horizontal"},'
    echo '{"key":"SPC w d","desc":"Delete window"},'
    echo '{"key":"SPC w w","desc":"Switch window"},'
    echo '{"key":"SPC w h/j/k/l","desc":"Navigate windows"},'
    echo '{"key":"SPC w =","desc":"Balance windows"}'
    echo ']},'

    echo '{"category":"Workspaces","items":['
    echo '{"key":"SPC TAB n","desc":"New workspace"},'
    echo '{"key":"SPC TAB d","desc":"Delete workspace"},'
    echo '{"key":"SPC TAB 1-9","desc":"Switch to workspace"},'
    echo '{"key":"SPC TAB [/]","desc":"Previous/Next workspace"}'
    echo ']},'

    echo '{"category":"Code & LSP","items":['
    echo '{"key":"gd","desc":"Go to definition"},'
    echo '{"key":"gr","desc":"Find references"},'
    echo '{"key":"K","desc":"Show documentation"},'
    echo '{"key":"SPC c a","desc":"Code action"},'
    echo '{"key":"SPC c r","desc":"Rename symbol"},'
    echo '{"key":"SPC c f","desc":"Format buffer"}'
    echo ']},'

    echo '{"category":"Git (Magit)","items":['
    echo '{"key":"SPC g g","desc":"Magit status"},'
    echo '{"key":"SPC g b","desc":"Blame"},'
    echo '{"key":"SPC g l","desc":"Log"},'
    echo '{"key":"SPC g d","desc":"Diff"},'
    echo '{"key":"SPC g s","desc":"Stage hunk"},'
    echo '{"key":"SPC g r","desc":"Revert hunk"}'
    echo ']},'

    echo '{"category":"Org Mode","items":['
    echo '{"key":"SPC n a","desc":"Org agenda"},'
    echo '{"key":"SPC X","desc":"Org capture"},'
    echo '{"key":"SPC m t","desc":"Toggle TODO"},'
    echo '{"key":"SPC m s","desc":"Schedule"},'
    echo '{"key":"SPC m d","desc":"Deadline"},'
    echo '{"key":"TAB","desc":"Fold/unfold heading"}'
    echo ']},'

    echo '{"category":"Terminal & Shell","items":['
    echo '{"key":"SPC o t","desc":"Toggle vterm"},'
    echo '{"key":"SPC o T","desc":"Open vterm here"}'
    echo ']}'

    echo "]"
}

# ── Firefox parser ───────────────────────────────────────────────────
parse_firefox() {
    echo "["

    echo '{"category":"Navigation","items":['
    echo '{"key":"Ctrl+L","desc":"Focus address bar"},'
    echo '{"key":"Ctrl+K","desc":"Focus search bar"},'
    echo '{"key":"Alt+←/→","desc":"Back / Forward"},'
    echo '{"key":"Ctrl+R","desc":"Reload page"},'
    echo '{"key":"Ctrl+Shift+R","desc":"Hard reload (no cache)"},'
    echo '{"key":"F5","desc":"Reload"},'
    echo '{"key":"Escape","desc":"Stop loading"}'
    echo ']},'

    echo '{"category":"Tabs","items":['
    echo '{"key":"Ctrl+T","desc":"New tab"},'
    echo '{"key":"Ctrl+W","desc":"Close tab"},'
    echo '{"key":"Ctrl+Shift+T","desc":"Reopen closed tab"},'
    echo '{"key":"Ctrl+Tab","desc":"Next tab"},'
    echo '{"key":"Ctrl+Shift+Tab","desc":"Previous tab"},'
    echo '{"key":"Ctrl+1-8","desc":"Switch to tab 1-8"},'
    echo '{"key":"Ctrl+9","desc":"Switch to last tab"}'
    echo ']},'

    echo '{"category":"Windows","items":['
    echo '{"key":"Ctrl+N","desc":"New window"},'
    echo '{"key":"Ctrl+Shift+N","desc":"New private window"},'
    echo '{"key":"F11","desc":"Toggle fullscreen"},'
    echo '{"key":"Ctrl+Shift+B","desc":"Toggle bookmarks toolbar"}'
    echo ']},'

    echo '{"category":"Find & Search","items":['
    echo '{"key":"Ctrl+F","desc":"Find in page"},'
    echo '{"key":"Ctrl+G / F3","desc":"Find next"},'
    echo '{"key":"Ctrl+Shift+G","desc":"Find previous"},'
    echo '{"key":"/","desc":"Quick find (text)"}'
    echo ']},'

    echo '{"category":"Page","items":['
    echo '{"key":"Ctrl+P","desc":"Print page"},'
    echo '{"key":"Ctrl+S","desc":"Save page"},'
    echo '{"key":"Ctrl+U","desc":"View source"},'
    echo '{"key":"Ctrl++/-/0","desc":"Zoom in/out/reset"},'
    echo '{"key":"Space / Shift+Space","desc":"Scroll down/up page"}'
    echo ']},'

    echo '{"category":"Developer Tools","items":['
    echo '{"key":"F12","desc":"Toggle DevTools"},'
    echo '{"key":"Ctrl+Shift+I","desc":"Inspector"},'
    echo '{"key":"Ctrl+Shift+J","desc":"Browser Console"},'
    echo '{"key":"Ctrl+Shift+M","desc":"Responsive Design Mode"}'
    echo ']}'

    echo "]"
}

# ── Main ─────────────────────────────────────────────────────────────
case "${1:-}" in
    sway)     parse_sway ;;
    wezterm)  parse_wezterm ;;
    bash)     parse_bash ;;
    emacs)    parse_emacs ;;
    firefox)  parse_firefox ;;
    *)
        echo "Usage: $0 {sway|wezterm|bash|emacs|firefox}" >&2
        exit 1
        ;;
esac
