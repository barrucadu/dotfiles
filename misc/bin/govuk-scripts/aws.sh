#!/usr/bin/env zsh

ENV=$1
COMMAND=$2
if [[ -z $ENV ]] || [[ -z $COMMAND ]]; then
  echo "(govuk docker) missing environment or subcommand"
  echo
  govuk help
  exit 1
fi
shift 2

AWS_PROFILE="govuk-${ENV}"

function __govuk_aws_assume {
  aws --profile $AWS_PROFILE sts assume-role \
      --role-arn $(crudini --get ~/.aws/config "profile ${AWS_PROFILE}" role_arn) \
      --role-session-name $USER
}

function __govuk_aws_get_credential_key {
  echo $credentials | ruby -e "require 'json'; c = JSON.parse(STDIN.read)['Credentials']; STDOUT << c['$1']"
}

case $COMMAND in
  'a'|'assume')
    credentials=$(__govuk_aws_assume) || return $?
    echo export AWS_ACCESS_KEY_ID=\'$(__govuk_aws_get_credential_key AccessKeyId)\'
    echo export AWS_SECRET_ACCESS_KEY=\'$(__govuk_aws_get_credential_key SecretAccessKey)\'
    echo export AWS_SESSION_TOKEN=\'$(__govuk_aws_get_credential_key SessionToken)\'
    echo export AWS_EXPIRATION=\'$(__govuk_aws_get_credential_key Expiration)\'
    ;;
  'e'|'exec')
    if [[ -z $1 ]]; then
      echo "(govuk aws ${ENV} exec) expected an argument"
      echo
      govuk help
      exit 1
    fi
    env "$(govuk aws ${ENV} assume)" "$@"
    ;;
  's'|'sh')
    PROMPT_TAG="aws:${ENV}" govuk aws exec zsh "$@"
    ;;
  'd'|'do')
    govuk aws exec aws "$@"
    ;;
  *)
    echo "(govuk aws ${ENV}) unknown subcommand '${COMMAND}'"
    echo
    govuk help
    ;;
esac
