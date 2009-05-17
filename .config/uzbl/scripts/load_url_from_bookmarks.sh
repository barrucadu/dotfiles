#!/bin/bash
bookmarks_file=$XDG_DATA_HOME/uzbl/bookmarks

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
OPTIONS=" -i -xs -rs -l 10"

goto=`< $bookmarks_file dmenu $OPTIONS $COLORS | cut -d ' ' -f -100  | awk '{print $NF}'`

[ -n "$goto" ] && uzblctrl -s $5 -c "act uri $goto"
