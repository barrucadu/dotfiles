## -*- shell-script -*-

# Source the system profile BEFORE setting up the user environment, so
# the PATH and stuff is configured properly. This is copied from the
# NixOS "zprofile_zwc_is_used" file.
if [[ -z $IN_NIX_SHELL ]]; then
  if test -e /etc/NIXOS; then
    if test -r /etc/zprofile; then
      . /etc/zprofile
    else
      emulate bash
      alias shopt=false
      . /etc/profile
      unalias shopt
      emulate zsh
    fi
    if test -r /etc/zprofile.local; then
      . /etc/zprofile.local
    fi
  else
    # on non-nixos we just source the global /etc/zprofile as if we did
    # not use the configure flag
    if test -r /etc/zprofile; then
      . /etc/zprofile
    fi
  fi
fi

### XDG User Dirs
export XDG_DESKTOP_DIR="$HOME/tmp"
export XDG_DOCUMENTS_DIR="$HOME/tmp"
export XDG_DOWNLOAD_DIR="$HOME/tmp"
export XDG_MUSIC_DIR="$HOME/nfs/music"

### Environment

export LEDGER_FILE="$HOME/s/ledger/current.journal"

export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

export GOPATH="$HOME/go"

#### Path

if [[ -z $DONE_PATH_MODIFICATIONS ]]; then
  # add MacTeX if it exists
  texpath="/usr/local/texlive/2016/bin/x86_64-darwin"
  if [[ -d $texpath ]]; then
    export PATH="$texpath:$PATH"
  fi

  # add /usr/local/go/bin if it exists
  gopath="/usr/local/go/bin"
  if [[ -d $gopath ]]; then
    export PATH="$gopath:$PATH"
  fi

  # add /usr/local/bin ahead of defaults
  export PATH="/usr/local/bin:$PATH"

  # add user-level python things
  pypath="$HOME/Library/Python/3.6/bin"
  if [[ -d $pypath ]]; then
    export PATH="$pypath:$PATH"
  fi

  # add things in ~ ahead of all
  export PATH="$GOPATH/bin:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH"

  # don't recursively mess up the PATH
  export DONE_PATH_MODIFICATIONS=true
fi

#### Programs

export BROWSER=chromium
export EDITOR=emacsclient
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=vim

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
