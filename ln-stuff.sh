#!/bin/zsh

CFGFILES=(uzbl)
HOMEFILES=(emacs irssi mostrc mplayer ncmpcpp stumpwmrc tmux.conf)

force=""
if [[ $1 == "-f" ]]; then
    echo "Forcing symlinks..."
    force=-f
fi

dir=`pwd`

for file in $CFGFILES; do
    ln -s $force $dir/config/$file $XDG_CONFIG_HOME/$file
done

for file in $HOMEFILES; do
    ln -s $force $dir/config/$file $HOME/.$file
done

ln -s $force $dir/config/xorg/XCompose $HOME/.XCompose
ln -s $force $dir/config/zsh/rc $HOME/.zshrc
ln -s $force $dir/bin $HOME/bin
ln -s $force $dir/.data $HOME/.data
ln -s $force $dir/code $HOME/code
