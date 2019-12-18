#!/usr/bin/env zsh

has() {
  type "$1" >/dev/null
}

short_env_name() {
  env="$1"
  case $env in
    "integration")    echo "i";;
    "staging")        echo "s";;
    "staging-aws")    echo "sa";;
    "production")     echo "p";;
    "production-aws") echo "pa";;
  esac
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
  "alias")
    echo "alias 'gk'='govuk'"
    echo "alias 'gka'='govuk aws'"
    for env in integration staging production; do
      short_env=$(short_env_name "$env")
      echo "alias 'gka${short_env}'='govuk aws ${env}'"
      for cmd in a d l; do
        echo "alias 'gka${short_env}${cmd}'='govuk aws ${env} ${cmd}'"
      done
    done
    echo "alias 'gkd'='govuk docker'"
    for cmd in b B r R c p '!' '?'; do
      echo "alias 'gkd${cmd}'='govuk docker ${cmd}'"
    done
    for db in e mo my p; do
      echo "alias 'gkds${db}'='govuk docker sync ${db}'"
    done
    echo "alias 'gkr'='govuk remote'"
    for env in integration staging staging-aws production production-aws; do
      short_env=$(short_env_name "$env")
      echo "alias 'gkr${short_env}'='govuk remote ${env}'"
      for cmd in c n s f; do
        echo "alias 'gkr${short_env}${cmd}'='govuk remote ${env} ${cmd}'"
      done
    done
  ;;
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
