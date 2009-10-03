#!/bin/bash

download=$6

if [[ ! -z $8 ]];
then
    download=$8
fi

pushd ~/tmp/downloads
wget --user-agent=Uzbl $download
popd