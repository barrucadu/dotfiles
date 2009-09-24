#!/usr/bin/env zsh

menupath=""
menufile=""
menutrack=""
choice=""

dmenunf="`xrdb -query | grep '*color14:' | sed 's/.*#/#/'`"
dmenusf="`xrdb -query | grep '*color10:' | sed 's/.*#/#/'`"
dmenunb="`xrdb -query | grep '*color0:' | sed 's/.*#/#/'`"
dmenusb="`xrdb -query | grep '*color0:' | sed 's/.*#/#/'`"

function resetmenu ()
{
    menupath="$HOME/menu/menus"
    menufile="main"
    menutrack="main"
    choice="main"
}

function getchoice ()
{
    menucontent=`cat $menupath/$menufile | sort && echo "Return"`
    choice="`echo $menucontent | dmenu -nf $dmenunf -nb $dmenunb -sf $dmenusf -sb $dmenusb | sed 's/ //g'`"
    
    [ "$choice" = "Return" ] && [ "$menufile" = "main" ] && exit
    [ "$choice" = "" ]       && exit

    menutrack="$menutrack-$choice:l"
    menufile="$choice:l"

    [ "$choice" = "Return" ] && resetmenu
}

resetmenu

while [[ -f $menupath/$choice:l ]]; do
    getchoice
done

command=`grep $menutrack $menupath/../commands`
cmd=`echo $command | sed "s/\([a-z0-9\-]*\) *//"`

echo $menupath
echo $menufile
echo $menutrack
echo $choice
echo $command

echo $cmd

setopt SH_WORD_SPLIT
$cmd &>/dev/null &
