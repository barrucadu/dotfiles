#!/usr/bin/env zsh

has() {
  type "$1" >/dev/null
}

COMMAND=$1
if [[ -z $COMMAND ]]; then
  echo "(govuk !) missing subcommand"
  echo
  govuk help
  exit 1
fi
shift

case $COMMAND in
  "check")
    ret=0
    if ! has docker; then
      echo '`docker` is not in $PATH'
      ret=1
    fi
    if ! has gds; then
      echo '`gds` is not in $PATH'
      ret=1
    fi
    if ! has govuk-docker; then
      echo '`govuk-docker` is not in $PATH'
      ret=1
    fi
    if ! has make; then
      echo '`make` is not in $PATH'
      ret=1
    fi
    exit $ret
    ;;
  *)
    echo "(govuk !) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
