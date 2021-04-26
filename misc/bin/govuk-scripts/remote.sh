#!/usr/bin/env zsh

# new (2021) laptop has a different local username
export USER=michaelswalker

ENV=$1
COMMAND=$2
if [[ -z $ENV ]] || [[ -z $COMMAND ]]; then
  echo "(govuk remote) missing environment or subcommand"
  echo
  govuk help
  exit 1
fi
shift 2

GCENV=$ENV
GCTY=carrenza

case "$ENV" in
  'integration')
    GCTY="aws"
    ;;
  'staging-aws')
    GCENV="staging"
    GCTY="aws"
    ;;
  'production-aws')
    GCENV="production"
    GCTY="aws"
    ;;
esac

case $COMMAND in
  'c'|'classes')
    govuk-connect ssh -e $GCENV "${GCTY}/jumpbox" -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no govuk_node_list --classes
    ;;
  'n'|'nodes')
    class=$1
    if [[ -z $class ]]; then
      govuk-connect ssh -e $GCENV "${GCTY}/jumpbox" -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no govuk_node_list
    else
      govuk-connect ssh -e $GCENV "${GCTY}/jumpbox" -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no govuk_node_list -c "$class"
    fi
    ;;
  's'|'ssh')
    class=$1
    if [[ -z $class ]]; then
      echo "(govuk remote ${ENV} ssh) expected an argument"
      echo
      govuk help
      exit 1
    fi
    shift
    govuk-connect ssh -e $GCENV "${GCTY}/${class}" -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$*"
    ;;
  'f'|'foreach')
    class=$1
    if [[ -z $class ]] || [[ -z $2 ]]; then
      echo "(govuk remote ${ENV} foreach) expected an argument"
      echo
      govuk help
      exit 1
    fi
    shift
    n=0
    for ip in $(govuk remote $ENV nodes $class); do
      n=$((n+1))
      govuk-connect ssh -e $GCENV "${GCTY}/${class}:${n}" -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$*"
    done
    ;;
  *)
    echo "(govuk remote ${ENV}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
