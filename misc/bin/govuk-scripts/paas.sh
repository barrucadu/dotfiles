#!/usr/bin/env zsh

LOC=$1
COMMAND=$2
if [[ -z $LOC ]] || [[ -z $COMMAND ]]; then
  echo "(govuk paas) missing location or subcommand"
  echo
  govuk help
  exit 1
fi
shift 2

case "$LOC" in
  'l'|'london')
    API="api.london.cloud.service.gov.uk"
    ;;
  'i'|'ireland')
    API="api.cloud.service.gov.uk"
    ;;
  *)
    echo "(govuk paas) unknown location '${LOC}'"
    echo
    govuk help
    exit 1
    ;;
esac

case $COMMAND in
  'l'|'login')
    cf login -a "$API" --sso
    ;;
  *)
    echo "(govuk paas ${LOC}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
