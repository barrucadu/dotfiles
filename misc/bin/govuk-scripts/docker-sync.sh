#!/usr/bin/env zsh

TARGET=$1
if [[ -z $TARGET ]]; then
  echo "(govuk docker sync) missing target"
  echo
  govuk help
  exit 1
fi
shift

CREDS=$(mktemp)
LOCAL_ARCHIVE_PATH="$HOME/govuk-data-sync"
trap "rm $CREDS" EXIT # FIXME: not executed on C-c

function aws_auth {
  govuk aws integration assume >> $CREDS
  source $CREDS
}

function wait_for_container {
  until [[ "$(docker inspect -f {{.State.Health.Status}} $1)" == "healthy" ]]; do
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
    mkdir -p $archive_path

    aws_auth
    local remote_config_paths=$(aws s3 ls "s3://${bucket}/" | grep -v '/$' | ruby -e 'STDOUT << STDIN.read.split("\n").map{|a| a.split(" ").last }.group_by { |n| n.split(/-\d/).first }.map { |_, d| d.sort.last.strip }.join(" ")')
    echo "REMOTE CONFIG PATHS: ${remote_config_paths}"
    for remote_config_path in "${(ps: :)remote_config_paths}"; do
      echo "Syncing data from ${remote_config_path}"
      aws_auth
      aws s3 cp "s3://${bucket}/${remote_config_path}" "${archive_path}/"
    done

    aws_auth
    local remote_file_details=$(aws s3 ls "s3://${bucket}/indices/")
    local remote_paths=$(echo "$remote_file_details" | ruby -e 'STDOUT << STDIN.read.split("PRE").group_by { |n| n.split(/-\d/).first }.map { |_, d| d.sort.last.strip }.join(" ")')
    for remote_path in "${(ps: :)remote_paths}"; do
      echo "Syncing data from ${remote_path}"
      aws_auth
      aws s3 sync "s3://${bucket}/indices/${remote_path}" "${archive_path}/indices/${remote_path}/"
    done
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
  wait_for_container $container

  local es=http://127.0.0.1:9200
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
  'es'|'elasticsearch')
    sync_elasticsearch 5
    sync_elasticsearch 6
    ;;
  'es5'|'elasticsearch5')
    sync_elasticsearch 5
    ;;
  'es6'|'elasticsearch6')
    sync_elasticsearch 6
    ;;
  *)
    echo "(govuk docker sync) unknown target '${TARGET}'"
    echo
    govuk help
    ;;
esac
