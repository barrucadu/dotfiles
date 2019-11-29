#!/usr/bin/env zsh

ENV=$1
COMMAND=$2
if [[ -z $ENV ]] || [[ -z $COMMAND ]]; then
  echo "(govuk aws) missing environment or subcommand"
  echo
  govuk help
  exit 1
fi
shift 2

ROLE="govuk-${ENV}-platformhealth-poweruser"

case $COMMAND in
  'a'|'assume')
    gds aws "$ROLE" -e
    ;;
  'd'|'do')
    gds aws "$ROLE" "$@"
    ;;
  *)
    echo "(govuk aws ${ENV}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
