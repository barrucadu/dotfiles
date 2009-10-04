#!/bin/bash

# search file in this line format: "engine searchuri" (must be formatted so querystr goes at end of searchuri)
search_file=$XDG_DATA_HOME/uzbl/search

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
OPTIONS=" -i -l 10"

engine=`sort $search_file | sed "s/\(.*\) .*/\\1/" | dmenu $OPTIONS $COLORS | cut -d ' ' -f -100  | awk '{print $NF}'`

if [[ "$engine" != "" ]]; then
    uri=`grep $engine $search_file | sed "s/.* //"`
    query=`dmenu $COLORS`
    
    if [[ "$query" != "" ]]; then
	goto="$uri$query"
	
	[ -n "$goto" ] && echo "uri $goto" > $4
    fi
fi
