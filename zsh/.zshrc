## -*- shell-script -*-

for file in ~/.zsh/*; do
  if [[ -f $file ]] && [[ -x $file ]]; then
    source $file
  fi
done
