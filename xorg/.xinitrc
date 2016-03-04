#!/usr/bin/env zsh
# xinitrc: read when `startx` is used

xrandr --output DVI-I-3 --rotate left --left-of DVI-I-0

setxkbmap gb
setxkbmap -option compose:rctrl
setxkbmap -option terminate:ctrl_alt_bksp

xrdb -load ~/.Xdefaults

xsetroot -cursor_name left_ptr

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

start-pulseaudio-x11

urxvtd -q -f -o

randombg

for winman in $WM herbstluftwm openbox twm; do
    if [[ -x `which $winman` ]]; then
        exec $winman
    fi
done
