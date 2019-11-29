#!/usr/bin/env zsh

TARGET=$1
if [[ -z $TARGET ]]; then
  echo "(govuk docker sync) missing target"
  echo
  govuk help
  exit 1
fi
shift

case $TARGET in
  'e'|'elasticsearch')
    govuk aws integration do $GOVUK_DOCKER_DIR/bin/replicate-elasticsearch.sh "$@"
    ;;
  'mo'|'mongodb')
    govuk aws integration do $GOVUK_DOCKER_DIR/bin/replicate-mongodb.sh "$@"
    ;;
  'my'|'mysql')
    govuk aws integration do $GOVUK_DOCKER_DIR/bin/replicate-mysql.sh "$@"
    ;;
  'p'|'postgresql')
    govuk aws integration do $GOVUK_DOCKER_DIR/bin/replicate-postgresql.sh "$@"
    ;;
  *)
    echo "(govuk docker sync) unknown target '${TARGET}'"
    echo
    govuk help
    ;;
esac
