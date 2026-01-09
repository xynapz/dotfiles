[[ -f ~/.profile ]] && emulate sh -c 'source ~/.profile'
export QT_QPA_PLATFORMTHEME=qt5ct
if [[ "$INSIDE_EMACS" ]]; then
    export TERM=foot
fi
