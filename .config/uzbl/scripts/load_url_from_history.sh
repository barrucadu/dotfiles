#!/bin/bash
history_file=$XDG_DATA_HOME/uzbl/history

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
OPTIONS=" -i -xs -rs -l 10"

goto=`< $history_file dmenu $OPTIONS $COLORS | cut -d ' ' -f -3  | awk '{print $NF}'`

[ -n "$goto" ] && uzblctrl -s $5 -c "uri $goto"
