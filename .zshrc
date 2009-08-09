## -*- shell-script -*-
# Additional files - I like splitting things up.
. ~/.config/zsh/profile
. ~/.config/zsh/functions
. ~/.config/zsh/aliases

if [[ ! $(tty) == *pts* ]]; then
    # Start screen or X
    if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty2 ]] && [[ "`runlevel`" == *5* ]]; then
        # Start X
        while true; do
            if [[ -f /var/run/daemons/hal ]]; then
                echo "Starting X11 at `date`"
                startx &> /dev/null
                rm .serverauth.*
                rm -rf /tmp/audacity*
            fi
        done
    elif [[ ! "$TERM" == "screen-256color" ]]; then
        # Start screen
        screen
    fi
    logout
else
    # Welcome message
    clear
    ddate +"It's %{%A, the %e of %B%}, %Y. %N%nCelebrate %H" | cowsay -f bunny.cow -W 50
    echo
    todo lt | sed "s/^.* TODO /To Do: /" | sort -R | head -n1
    echo
fi
