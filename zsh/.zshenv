## -*- shell-script -*-

### XDG User Dirs
export XDG_DESKTOP_DIR="$HOME/tmp"
export XDG_DOCUMENTS_DIR="$HOME/tmp"
export XDG_DOWNLOAD_DIR="$HOME/tmp"
export XDG_MUSIC_DIR="$HOME/nfs/music"

### Environment

export LEDGER_FILE="$HOME/.hledger.journals/current.journal"

export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

#### Path

export GOPATH="$HOME/go"

# add /use/local/go/bin
gopath="/usr/local/go/bin"
if [[ -d $gopath ]] && [[ "${PATH#*$gopath}" == "$PATH" ]]; then
  export PATH="$gopath:$PATH"
fi

# add /usr/local/bin ahead of defaults
localpath="/usr/local/bin"
if [[ "${PATH#*$localpath}" == "$PATH" ]]; then
  export PATH="$localpath:$PATH"
fi

# add things in ~ ahead of all
newpath="$GOPATH/bin:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin"
if [[ "${PATH#*$newpath}" == "$PATH" ]]; then
  export PATH="$newpath:$PATH"
fi

#### Programs

export BROWSER=chromium
export EDITOR=vim
export VISUAL=$EDITOR

#### Locale

export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

### Emacs shell mode
[[ $EMACS = t ]] && unsetopt zle

### Operating system

if [[ `uname -o 2>/dev/null` == "GNU/Linux" ]]; then
  export IS_GNULINUX=true
else
  export IS_GNULINUX=false
fi

if [[ `uname -s` == "Darwin" ]]; then
  export IS_OSX=true
else
  export IS_OSX=false
fi
