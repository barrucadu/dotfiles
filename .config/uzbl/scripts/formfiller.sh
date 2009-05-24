#!/bin/bash

# simple login form filler for uzbl.
# put the form entry values you want to add (eg login information) in the file $keydir/<domain>
# in the format <fieldname>: <value>
# (these files can be automatically created for you by setting editor and triggering this script on a site without a config)

keydir=$XDG_DATA_HOME/uzbl/forms

editor='urxvt -e emacs'

config=$1;
pid=$2;
xid=$3;
fifo=$4;
socket=$5;
url=$6;
title=$7;

[ -d $keydir ] || mkdir $keydir || exit 1

domain=$(echo $url | sed -re 's|(http\|https)+://(www\.\|)([A-Za-z0-9\.]+)/.*|\3|')

echo $keydir/$domain

if [[ -e $keydir/$domain ]]; then
	gawk -F': ' '{ print "act js document.getElementsByName(\"" $1 "\")[0].value = \"" $2 "\";"}' $keydir/$domain >> $fifo
else
	curl "$url" | grep '<input' | sed -nre 's|.*<input.*?name="([[:graph:]]+)".*?/>.*|\1: |p' > $keydir/$domain
	$editor $keydir/$domain
fi
