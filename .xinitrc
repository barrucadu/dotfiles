#!/bin/zsh

cmdline="`cat /proc/cmdline`"

function background()
{
    $* &>/dev/null &
}

function notinschool()
{
    if [[ ! "$cmdline" == *school* ]]; then
        background $*
    fi
}

function inschool()
{
    if [[ "$cmdline" == *school* ]]; then
        background $*
    fi
}

if [[ -f /var/run/daemons/hal ]]; then
    # X
        # Keyboard
        setxkbmap gb
        setxkbmap -option compose:menu
        setxkbmap -option terminate:ctrl_alt_bksp
        xmodmap -e "clear Mod4"

        # Touchpad
        synclient MinSpeed=0.10
        synclient MaxSpeed=0.80
        synclient AccelFactor=0.50
        synclient VertEdgeScroll=1
        synclient HorizEdgeScroll=1

        # Other
        xsetroot -cursor_name left_ptr
        xrdb -load ~/.Xdefaults
    # /X

    # Services
    urxvtd -q -f -o
    notinschool xscreensaver -no-splash
    notinschool mpddp
    notinschool ucron
    
    # Appearence
    notinschool /home/barrucadu/bin/randombg
    inschool feh --bg-scale /usr/share/wallpapers/Anime/flcl04.png
    
    # Window Manager
    exec stumpwm
fi
