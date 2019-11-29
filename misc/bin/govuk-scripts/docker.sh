#!/usr/bin/env zsh

COMMAND=$1
if [[ -z $COMMAND ]]; then
  echo "(govuk docker) missing subcommand"
  echo
  govuk help
  exit 1
fi
shift

case $COMMAND in
  'b'|'build')
    if [[ -z $1 ]]; then
      echo "(govuk docker build) expected an argument"
      echo
      govuk help
      exit 1
    fi
    make -f "$GOVUK_DOCKER_DIR/Makefile" "$@"
    ;;
  'B'|'build-this')
    this=$(basename $(pwd))
    make -f "$GOVUK_DOCKER_DIR/Makefile" "$this"
    ;;
  'r'|'run')
    app=$1
    stack=$2
    if [[ -z $app ]]; then
      echo "(govuk docker run) expected an argument"
      echo
      govuk help
      exit 1
    fi
    shift
    if [[ -z $stack ]]; then
      govuk-docker run "${app}-lite"
    else
      shift
      govuk-docker run "${app}-${stack}" "$@"
    fi
    ;;
  'R'|'run-this')
    this=$(basename $(pwd))
    stack=$1
    if [[ -z $stack ]]; then
      govuk-docker run "${this}-lite"
    else
      shift
      govuk-docker run "${this}-${stack}" "$@"
    fi
    ;;
  'c'|'compose')
    if [[ -z "$1" ]]; then
      echo "(govuk docker compose) expected an argument"
      echo
      govuk help
      exit 1
    fi
    govuk-docker "$@"
    ;;
  'p'|'prune')
    docker system  prune -f
    docker network prune -f
    docker volume  prune -f
    ;;
  's'|'sync')
    $GOVUK_SCRIPT_DIR/docker-sync.sh "$@"
    ;;
  '!'|'stop')
    govuk-docker stop
    ;;
  '?'|'stacks')
    pat=$1
    govuk-docker ps --services | grep "$pat"
    ;;
  *)
    echo "(govuk docker) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
