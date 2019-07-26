#!/usr/bin/env zsh

ENV=$1
COMMAND=$2
if [[ -z $ENV ]] || [[ -z $COMMAND ]]; then
  echo "(govuk remote) missing environment or subcommand"
  echo
  govuk help
  exit 1
fi
shift 2

case $COMMAND in
  'c'|'classes')
    ssh $ENV "govuk_node_list --classes"
    ;;
  'n'|'nodes')
    class=$1
    if [[ -z $class ]]; then
      ssh $ENV govuk_node_list
    else
      ssh $ENV "govuk_node_list -c $2"
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
    ssh $(ssh $ENV "govuk_node_list --single-node -c ${class}").$ENV "$@"
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
    for ip in $(govuk remote nodes $env $class); do
      ssh "${IP}.${ENV}" "$@"
    done
    ;;
  *)
    echo "(govuk remote ${ENV}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
