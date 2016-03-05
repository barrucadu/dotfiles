#!/usr/bin/env zsh
# xinitrc: read when `startx` is used

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

source ~/.xprofile

if [[ ! -z $WM ]]; then
  exec $WM
else
  for winman in herbstluftwm openbox twm; do
    if [[ -x `which $winman` ]]; then
      exec $winman
    fi
  done
fi
