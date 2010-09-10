#!/bin/zsh

CFGFILES=(uzbl)
HOMEFILES=(emacs irssi mostrc mplayer ncmpcpp stumpwmrc tmux.conf)

for file in $CFGFILES; do
    ln -s $file ~/.config/$file
fi

for file in $CFGFILES; do
    ln -s $file ~/.$file
fi

ln -s zsh/rc ~/.zshrc