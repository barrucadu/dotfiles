#!/bin/zsh

CFGFILES=()
HOMEFILES=(emacs mplayer ncmpcpp stumpwmrc tmux.conf)

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

mkdir $HOME/bin
pushd $HOME/bin
for file in $dir/bin/*; do
    ln -s $force $file .
done
popd

mkdir $HOME/.xmonad
ln -s $force $dir/config/xmonad.hs $HOME/.xmonad/xmonad.hs
