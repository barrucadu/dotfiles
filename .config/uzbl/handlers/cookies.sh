#!/bin/zsh

file=$XDG_CONFIG_HOME/uzbl/cookies
cookie_dir=$XDG_DATA_HOME/uzbl/cookies

which zenity &>/dev/null || exit 2

uri=$6
uri=${uri/http:\/\/} # strip 'http://' part
action=$8 # GET/PUT
cookie=$9
host=${uri/\/*/}

function match () {
	sed -n "/$1/,/^\$/p" $file 2>/dev/null | grep -q "^$host"
}

function fetch_cookie () {
	cookie=`cat $cookie_dir/$host.cookie`
}

function store_cookie () {
	echo $cookie > $cookie_dir/$host.cookie
}

if match TRUSTED $host
then
	[ $action == PUT ] && store_cookie $host
	[ $action == GET ] && fetch_cookie && echo "$cookie"
elif ! match DENY $host
then
	[ $action == PUT ] &&                 cookie=`zenity --entry --title 'Uzbl Cookie handler' --text "Accept this cookie from $host ?" --entry-text="$cookie"` && store_cookie $host
	[ $action == GET ] && fetch_cookie && cookie=`zenity --entry --title 'Uzbl Cookie handler' --text "Submit this cookie to $host ?"   --entry-text="$cookie"` && echo $cookie
fi
exit 0
