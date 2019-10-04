#!/usr/bin/env zsh

COMMAND=$1
if [[ -z $COMMAND ]]; then
  echo "(govuk docker) missing subcommand"
  echo
  govuk help
  exit 1
fi
shift

# doesn't get exported from the larger environment
eval "$(rbenv init - zsh)"

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
      GOVUK_DOCKER_SERVICE="$app" govuk-docker run
    else
      shift
      GOVUK_DOCKER_SERVICE="$app" govuk-docker run --stack $stack "$@"
    fi
    ;;
  'R'|'run-this')
    stack=$1
    if [[ -z $stack ]]; then
      govuk-docker run
    else
      shift
      govuk-docker run --stack $stack "$@"
    fi
    ;;
  'c'|'compose')
    if [[ -z "$1" ]]; then
      echo "(govuk docker compose) expected an argument"
      echo
      govuk help
      exit 1
    fi
    govuk-docker compose "$@" | tail -n+2
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
    govuk docker compose stop
    ;;
  '?'|'stacks')
    pat=$1
    govuk docker compose ps --services | grep "$pat"
    ;;
  *)
    echo "(govuk docker) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
