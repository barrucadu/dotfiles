#!/bin/bash
bookmarks_file=$XDG_DATA_HOME/uzbl/bookmarks
bookmarks_tags=$XDG_DATA_HOME/uzbl/bookmarks.tags

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
OPTIONS=" -i -xs -rs -l 10"

tag=`< $bookmarks_tags dmenu $OPTIONS $COLORS`

if [[ -n $tag ]];
then
    echo "[$tag] $7: $6" >> $bookmarks_file
fi
