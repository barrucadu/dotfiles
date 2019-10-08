#!/usr/bin/env zsh

TARGET=$1
if [[ -z $TARGET ]]; then
  echo "(govuk docker sync) missing target"
  echo
  govuk help
  exit 1
fi
shift

LOCAL_ARCHIVE_PATH="$HOME/govuk-data-sync"

function wait_for_http {
  until curl $1 &>/dev/null; do
    sleep 0.1
  done
}

function sync_elasticsearch {
  local ver=$1
  local bucket="govuk-integration-elasticsearch${ver}-manual-snapshots"
  local archive_path="$LOCAL_ARCHIVE_PATH/elasticsearch-${ver}"

  echo "Replicating elasticsearch ${ver}"

  if [[ -e $archive_path ]]; then
    echo "Skipping download - remove ${archive_path} to force"
  else
    govuk aws integration do s3 sync "s3://${bucket}/" "${archive_path}/"
  fi

  # temporary config file because ES needs to be configured in advance
  # for filesystem-based snapshots
  local cfg_path=$(mktemp '/tmp/govuk-docker-data-sync.XXXXX')
  trap "rm $cfg_path" EXIT # FIXME: not executed on C-c
  echo "
    cluster.name: 'docker-cluster'
    network.host: 0.0.0.0

    discovery.zen.minimum_master_nodes: 1
    path.repo: ['/replication']
  " > $cfg_path

  local container=$(govuk docker compose run -d --rm -v $archive_path:/replication -v $cfg_path:/usr/share/elasticsearch/config/elasticsearch.yml -p 9200:9200 "elasticsearch${ver}")
  trap "docker stop $container" EXIT # FIXME: not executed on C-c

  local es=http://127.0.0.1:9200
  wait_for_http $es
  curl -XDELETE "${es}/_all"
  curl "${es}/_snapshot/snapshots" -X PUT -H 'Content-Type: application/json' -d '{
    "type": "fs",
    "settings": {
      "compress": true,
      "readonly": true,
      "location": "/replication"
    }
  }'
  sleep 1
  local snapshot_name=$(curl "${es}/_snapshot/snapshots/_all" | ruby -e 'require "json"; STDOUT << (JSON.parse(STDIN.read)["snapshots"].map { |a| a["snapshot"] }.sort.last)')
  curl -XPOST "${es}/_snapshot/snapshots/${snapshot_name}/_restore?wait_for_completion=true"
}

case $TARGET in
  'es6'|'elasticsearch6')
    sync_elasticsearch 6
    ;;
  *)
    echo "(govuk docker sync) unknown target '${TARGET}'"
    echo
    govuk help
    ;;
esac
