## -*- shell-script -*-

. ~/.zsh/profile
. ~/.zsh/functions
. ~/.zsh/aliases

# Start X
if [[ -z "$DISPLAY" ]] && [[ $(tty) == /dev/tty2 ]]; then
    while [[ ! -e ~/.noX ]]; do
        echo "Starting X11"
        xinit &>/dev/null
        source ~/.zsh/profile
    done
fi
