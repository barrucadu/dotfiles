#!/bin/zsh

HOMEFILES=(emacs mplayer ncmpcpp tmux.conf zsh gitconfig)

force=""
if [[ $1 == "-f" ]]; then
    echo "Forcing symlinks..."
    force=-f
fi

dir=`pwd`

for file in $HOMEFILES; do
    ln -s $force $dir/config/$file $HOME/.$file
done

pushd $dir/config/xorg
for file in *; do
    ln -s $force $dir/config/xorg/$file $HOME/.$file
done
popd

ln -s $force $dir/config/zsh/rc $HOME/.zshrc
ln -s $force $dir/config/zsh/profile $HOME/.zprofile

mkdir $HOME/bin
pushd $HOME/bin
for file in $dir/bin/*; do
    ln -s $force $file .
done
popd

mkdir $HOME/.xmonad
ln -s $force $dir/config/xmonad.hs $HOME/.xmonad/xmonad.hs

mkdir $HOME/.ssh
ln -s $force $dir/config/ssh-config $HOME/.ssh/config

pushd $dir/config/emacs.d
mkdir $HOME/.emacs.d
for file in $; do
    ln -s $force $dir/config/emacs.d/$file $HOME/.emacs.d/$file
done
popd
