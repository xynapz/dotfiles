#!/usr/bin/env bash
# Wallpaper Selector - Repo-based browsing with back navigation
# Flow: Select Repo → Browse Images → Set as Home/Lock/Both
# Uses: fuzzel, swaymsg, fd

set -euo pipefail

WALLPAPER_DIR="$HOME/Pictures/Wallpapers"
REPOS_FILE="$HOME/dotfiles/scripts/wallpaper_repos"
HOME_WALLPAPER="$HOME/.wallpaper-home"
LOCK_WALLPAPER="$HOME/.wallpaper-lock"
BACK="← Back"

# ── Ensure wallpaper repos are cloned ──────────────────────────────────
setup_repos() {
    mkdir -p "$WALLPAPER_DIR"
    [[ ! -f "$REPOS_FILE" ]] && return

    while IFS= read -r repo_url; do
        [[ -z "$repo_url" || "$repo_url" =~ ^# ]] && continue
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

# ── Find all images in a directory recursively ─────────────────────────
find_images_in() {
    local dir="$1"
    fd -t f -e jpg -e jpeg -e png -e webp -e bmp . "$dir" \
        --exclude '.git' 2>/dev/null | \
        sed "s|^$dir/||" | sort
}

# ── Apply wallpaper ────────────────────────────────────────────────────
apply_home() {
    ln -sf "$1" "$HOME_WALLPAPER"
    swaymsg output '*' bg "$1" fill 2>/dev/null || true
    notify-send "🖼 Home Wallpaper" "$(basename "$1")" -t 2000
}

apply_lock() {
    ln -sf "$1" "$LOCK_WALLPAPER"
    notify-send "🔒 Lock Wallpaper" "$(basename "$1")" -t 2000
}

# ── Target picker ─────────────────────────────────────────────────────
pick_target() {
    local full_path="$1"

    local target
    target=$(printf "🖼  Home Screen\n🔒 Lock Screen\n🎨 Both\n%s" "$BACK" | \
        fuzzel -d -p "Set as: " -w 40) || exit 0

    [[ -z "$target" ]] && exit 0

    case "$target" in
        *Back*)  return 1 ;;  # Signal to go back
        *Home*)  apply_home "$full_path" ;;
        *Lock*)  apply_lock "$full_path" ;;
        *Both*)  apply_home "$full_path"; apply_lock "$full_path" ;;
    esac
    return 0
}

# ── Image browser within a repo ───────────────────────────────────────
browse_repo() {
    local repo_name="$1"
    local repo_path="$WALLPAPER_DIR/$repo_name"

    while true; do
        local images
        images=$(find_images_in "$repo_path")

        if [[ -z "$images" ]]; then
            notify-send "Wallpaper" "No images found in $repo_name"
            return 1  # Go back to repo list
        fi

        local selected
        selected=$(printf "%s\n%s" "$BACK" "$images" | \
            fuzzel -d -p "$repo_name: " -w 80) || return 0

        [[ -z "$selected" ]] && return 0

        # Back to repo list
        [[ "$selected" == "$BACK" ]] && return 1

        local full_path="$repo_path/$selected"
        [[ ! -f "$full_path" ]] && { notify-send "Wallpaper Error" "File not found"; continue; }

        # Pick target — if user goes back, re-show image list
        pick_target "$full_path" || continue

        # Applied successfully, exit
        return 0
    done
}

# ── Main: repo selector ───────────────────────────────────────────────
main() {
    setup_repos

    # Get list of repos (directories in wallpaper dir)
    while true; do
        local repos
        repos=$(find "$WALLPAPER_DIR" -mindepth 1 -maxdepth 1 -type d \
            -not -name '.git' -printf '%f\n' 2>/dev/null | sort)

        if [[ -z "$repos" ]]; then
            notify-send "Wallpaper" "No wallpaper collections found"
            exit 1
        fi

        # Count images per repo for display
        local repo_list=""
        while IFS= read -r repo; do
            local count
            count=$(fd -t f -e jpg -e jpeg -e png -e webp -e bmp . \
                "$WALLPAPER_DIR/$repo" --exclude '.git' 2>/dev/null | wc -l)
            repo_list+="$repo ($count images)"$'\n'
        done <<< "$repos"

        local selected
        selected=$(echo "$repo_list" | fuzzel -d -p "Collection: " -w 60) || exit 0

        [[ -z "$selected" ]] && exit 0

        # Extract repo name (strip the count suffix)
        local repo_name
        repo_name=$(echo "$selected" | sed 's/ ([0-9]* images)$//')

        # Browse that repo — if browse returns 1, user wants to go back
        browse_repo "$repo_name" || continue

        # Applied successfully
        break
    done
}

main "$@"
