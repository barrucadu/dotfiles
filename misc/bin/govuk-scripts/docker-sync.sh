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
trap "rm $CREDS" EXIT

function aws_auth {
  govuk aws integration assume >> $CREDS
  source $CREDS
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

  # `docker cp` reliably hangs after copying 6.4GB, so instead of
  # using that spin up a container with the host directory
  # bind-mounted and run `cp` inside the container.
  govuk docker compose run --rm -v $archive_path:/import "elasticsearch${ver}" bash -xc "
    [[ -e '/replication/index.latest' ]] && rm -r '/replication/*'
    cp -a /import/* /replication
  "

  govuk docker run search-api lite bash -xc "
    es='http://elasticsearch${ver}:9200'
    curl -XDELETE \"\${es}/_all\"
    curl \"\${es}/_snapshot/snapshots\" -X PUT -H 'Content-Type: application/json' -d '{
      \"type\": \"fs\",
      \"settings\": {
        \"compress\": true,
        \"readonly\": true,
        \"location\": \"/replication\"
      }
    }'

    snapshot_name=\$(curl \"\${es}\"/_snapshot/snapshots/_all | ruby -e 'require \"json\"; STDOUT << (JSON.parse(STDIN.read)[\"snapshots\"].map { |a| a[\"snapshot\"] }.sort.last)')
    curl -XPOST \"\${es}/_snapshot/snapshots/\${snapshot_name}/_restore?wait_for_completion=true\"
  "
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
