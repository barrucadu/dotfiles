## -*- shell-script -*-

# XDG User Dirs
export XDG_DESKTOP_DIR="$HOME/tmp"
export XDG_DOCUMENTS_DIR="$HOME/tmp"
export XDG_DOWNLOAD_DIR="$HOME/tmp"
export XDG_MUSIC_DIR="$HOME/nfs/music"

# Environment
export GOPATH="$HOME/go"

newpath="$GOPATH/bin:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin"
if [[ "${PATH#*$newpath}" == "$PATH" ]]; then
  export PATH="$newpath:$PATH"
fi

export BROWSER=chromium
export EDITOR=vim
export VISUAL=$EDITOR

export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

# Emacs shell mode
[[ $EMACS = t ]] && unsetopt zle
