#!/bin/zsh

cmdline="`cat /proc/cmdline`"

function notinschool()
{
    if [[ ! "$cmdline" == *school* ]]; then
        $* &>/dev/null &
    fi
}

function inschool()
{
    if [[ "$cmdline" == *school* ]]; then
        $* &>/dev/null &
    fi
}

if [[ -f /var/run/daemons/hal ]]; then
    # X
    setxkbmap -option compose:menu
    setxkbmap -option terminate:ctrl_alt_bksp
    xmodmap -e "clear Mod4"
    xrdb -load ~/.Xdefaults

    # Services
    pcmanfm -d &
    urxvtd -q -f -o
    notinschool xscreensaver -no-splash
    notinschool mpddp
    notinschool ucron
    
    # Appearence
    notinschool /home/barrucadu/bin/randombg
    #notinschool /home/barrucadu/bin/fortunepaper
    inschool feh --bg-scale "/usr/share/wallpapers/Fire/Burnface.jpg"
    
    # Window Manager
    exec stumpwm
fi
