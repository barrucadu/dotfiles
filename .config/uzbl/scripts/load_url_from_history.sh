#!/bin/zsh

history_file=$XDG_DATA_HOME/uzbl/history
current=`tail -n 1 $history_file | awk '{print $3}'`
goto=`(echo $current; awk '{print $3}' $history_file | grep -v "^$current\$" | sort -u) | dmenu -i`

[ -n "$goto" ] && uzblctrl -s $5 -c "uri $goto"
