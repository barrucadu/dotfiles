## -*- shell-script -*-

# Programs
export EDITOR=emacs
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=vim

## Options
export LEDGER_FILE="$HOME/s/ledger/current.journal"
export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
export LESS="-FXRS"
export FZF_DEFAULT_OPTS="--height=25% --min-height=15 --reverse"

# Locale
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

# XDG user dirs
export XDG_DESKTOP_DIR="$HOME/tmp"
export XDG_DOCUMENTS_DIR="$HOME/tmp"
export XDG_DOWNLOAD_DIR="$HOME/tmp"

# Miscellaneous
export KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')

if [[ -e ~/.zshenv.host ]]; then
  source ~/.zshenv.host
fi
