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

ROLE="govuk-${ENV}-poweruser"

if [[ "$AWS_ADMIN" == "1" ]]; then
  ROLE="govuk-${ENV}-admin"
fi

case $COMMAND in
  'a'|'assume')
    gds aws "$ROLE" -e
    ;;
  'l'|'login')
    gds aws "$ROLE" -l
    ;;
  'd'|'do')
    gds aws "$ROLE" "$@"
    ;;
  't'|'terraform')
    project="$1"
    subcmd="$2"
    if [[ -z "$project" ]]; then
      echo "(govuk aws ${ENV} terraform) expected a project name"
      echo
      govuk help
      exit 1
    fi
    if [[ "$project" =~ ^infra-.*  ]]; then
      stack="govuk"
    else
      stack="blue"
    fi
    case $subcmd in
      'p'|'plan')
        gds govuk terraform -e "$ENV" -p "$project" -s "$stack" -a plan -r "$ROLE"
        ;;
      'a'|'apply')
        gds govuk terraform -e "$ENV" -p "$project" -s "$stack" -a apply -r "$ROLE"
        ;;
      *)
        echo "(govuk aws ${ENV} terraform ${project}) unknown subcommand '${subcmd}'"
        echo
        govuk help
        ;;
    esac
    ;;
  *)
    echo "(govuk aws ${ENV}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
