#!/bin/zsh

download=$6

if [[ ! -z $8 ]];
then
    download=$8
fi

if [[ $download =~ .*(.torrent) ]] || [[ $download == *mininova* ]];
then
    pushd /srv/torrents
else
    pushd ~/tmp/downloads
fi

wget --user-agent=Uzbl $download
popd