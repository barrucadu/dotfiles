#!/bin/bash
bookmarks_file=$XDG_DATA_HOME/uzbl/bookmarks
bookmarks_tags=$XDG_DATA_HOME/uzbl/bookmarks.tags

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
OPTIONS=" -i -xs -rs -l 10"

tag=`< $bookmarks_tags dmenu $OPTIONS $COLORS`

if [[ -n $tag ]] && [[ -n $7 ]] && [[ -n $6 ]];
then
    if grep $tag $bookmarks_tags &>/dev/null;
    then
        echo "Tag found in $bookmarks_tags" > /dev/null # NOP :)
    else
        # Add tag
        echo $tag >> $bookmarks_tags
        sort $bookmarks_tags > /tmp/tags
        mv /tmp/tags $bookmarks_tags
    fi

    echo "[$tag] $7: $6" >> $bookmarks_file
fi
