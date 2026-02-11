#!/usr/bin/env bash
# Wallpaper Selector - Browse repos, pick wallpapers, set home or lock screen
# Uses: fuzzel, swaymsg, fd, swaylock

set -euo pipefail

WALLPAPER_DIR="$HOME/Pictures/Wallpapers"
REPOS_FILE="$HOME/dotfiles/wallpaper_repos"
HOME_WALLPAPER="$HOME/.wallpaper-home"
LOCK_WALLPAPER="$HOME/.wallpaper-lock"
IMAGE_EXTENSIONS="jpg|jpeg|png|webp|bmp"

# ── Ensure wallpaper repos are cloned ──────────────────────────────────
setup_repos() {
    mkdir -p "$WALLPAPER_DIR"

    if [[ ! -f "$REPOS_FILE" ]]; then
        notify-send "Wallpaper" "No repos file found at $REPOS_FILE"
        return
    fi

    while IFS= read -r repo_url; do
        # Skip empty lines and comments
        [[ -z "$repo_url" || "$repo_url" =~ ^# ]] && continue

        # Extract repo name from URL (handles both ssh and https)
        repo_name=$(basename "$repo_url" .git)
        repo_path="$WALLPAPER_DIR/$repo_name"

        if [[ ! -d "$repo_path" ]]; then
            notify-send "Wallpaper" "Cloning $repo_name..." -t 3000
            git clone --depth 1 "$repo_url" "$repo_path" 2>/dev/null || {
                notify-send "Wallpaper Error" "Failed to clone $repo_name"
            }
        fi
    done < "$REPOS_FILE"
}

# ── Find all images recursively, display relative paths ────────────────
find_images() {
    fd -t f -e jpg -e jpeg -e png -e webp -e bmp . "$WALLPAPER_DIR" \
        --exclude '.git' \
        2>/dev/null | \
        sed "s|^$WALLPAPER_DIR/||" | \
        sort
}

# ── Apply wallpaper ────────────────────────────────────────────────────
apply_home() {
    local img="$1"
    ln -sf "$img" "$HOME_WALLPAPER"
    swaymsg output '*' bg "$img" fill 2>/dev/null || true
    notify-send "🖼 Home Wallpaper" "$(basename "$img")" -t 2000
}

apply_lock() {
    local img="$1"
    ln -sf "$img" "$LOCK_WALLPAPER"
    notify-send "🔒 Lock Wallpaper" "$(basename "$img")" -t 2000
}

apply_both() {
    local img="$1"
    apply_home "$img"
    apply_lock "$img"
}

# ── Main flow ──────────────────────────────────────────────────────────
main() {
    # Ensure repos exist
    setup_repos

    # Find all images and let user pick
    local images
    images=$(find_images)

    if [[ -z "$images" ]]; then
        notify-send "Wallpaper" "No images found in $WALLPAPER_DIR"
        exit 1
    fi

    local selected
    selected=$(echo "$images" | fuzzel -d -p "Wallpaper: " -w 80)

    [[ -z "$selected" ]] && exit 0

    local full_path="$WALLPAPER_DIR/$selected"

    if [[ ! -f "$full_path" ]]; then
        notify-send "Wallpaper Error" "File not found: $selected"
        exit 1
    fi

    # Ask where to apply
    local target
    target=$(printf "🖼  Home Screen\n🔒 Lock Screen\n🎨 Both" | fuzzel -d -p "Set as: " -w 40)

    [[ -z "$target" ]] && exit 0

    case "$target" in
        *Home*)  apply_home "$full_path" ;;
        *Lock*)  apply_lock "$full_path" ;;
        *Both*)  apply_both "$full_path" ;;
    esac
}

main "$@"
