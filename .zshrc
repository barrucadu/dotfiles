## -*- shell-script -*-
# Additional files - I like splitting things up.
. ~/.zsh_profile
. ~/.zsh_functions
. ~/.zsh_aliases

# Keybindings - I'm not actually sure which ones are required.
bindkey "\e[1~"  beginning-of-line
bindkey "\e[4~"  end-of-line
bindkey "\e[5~"  beginning-of-history
bindkey "\e[6~"  end-of-history
bindkey "\e[3~"  delete-char
bindkey "\e[2~"  quoted-insert
bindkey "\e[5C"  forward-word
bindkey "\eOc"   emacs-forward-word
bindkey "\e[5D"  backward-word
bindkey "\eOd"   emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "^H"     backward-delete-word
bindkey "\e[8~"  end-of-line
bindkey "\e[7~"  beginning-of-line
bindkey "\eOH"   beginning-of-line
bindkey "\eOF"   end-of-line
bindkey "\e[H"   beginning-of-line
bindkey "\e[F"   end-of-line
bindkey '^i'     expand-or-complete-prefix

# Colourful Directory Listings
eval `dircolors -b`

# Tab Completion
zle -C complete-file complete-word _generic

zstyle ':completion:*'                 completer _complete _match _approximate
zstyle ':completion:*:match:*'         original only
zstyle ':completion:*:approximate:*'   max-errors 1 numeric
zstyle ':completion:*:*'               original only
zstyle ':completion:complete-file::::' completer _files
zstyle ':completion:*'                 completer _complete _ignored _files

autoload -Uz compinit
compinit

# Misc Settings
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt autocd
setopt extendedglob
setopt correct
setopt extended_history

# Do some stuff if in a TTY.
if [[ ! $(tty) == *pts* ]]; then
	. ~/.zsh_login
fi

# Welcome message
clear
ddate +"It's %{%A, the %e of %B%}, %Y. %N%nCelebrate %H" | cowsay -f bunny.cow -W 50
echo
