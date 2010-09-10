#!/bin/bash

COLORS=" -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
goto=`dmenu $COLORS`
[ -n "$goto" ] && echo "uri $goto" > $4
