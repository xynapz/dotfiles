# Shell functions
# Sourced by shell.nix

mkcd() { mkdir -p "$1" && cd "$1"; }

extract() {
  [ -f "$1" ] || { echo "'$1' is not a valid file"; return; }
  case "$1" in
    *.tar.bz2) tar xjf "$1" ;; *.tar.gz) tar xzf "$1" ;; *.tar.xz) tar xJf "$1" ;;
    *.bz2) bunzip2 "$1" ;; *.gz) gunzip "$1" ;; *.tar) tar xf "$1" ;;
    *.zip) unzip "$1" ;; *.7z) 7z x "$1" ;; *) echo "'$1' cannot be extracted" ;;
  esac
}

ff() { fd --type f "$1" 2>/dev/null || find . -type f -iname "*$1*" 2>/dev/null; }
fdir() { fd --type d "$1" 2>/dev/null || find . -type d -iname "*$1*" 2>/dev/null; }

up() {
  local count="${1:-1}"; local path=""
  for ((i=0;i<count;i++)); do path="../$path"; done
  cd "$path" || return
}

preview() { bat --style=numbers --color=always "${1:--}" | head -${2:-100}; }
