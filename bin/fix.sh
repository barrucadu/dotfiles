#!/bin/zsh

function showhelp()
{
    echo "fix.sh - script for attempting to fix completely hosed Arch installations."
    echo
    echo "-m     Fix the pacman mirrorlist"
    echo "-p     Manually reinstall pacman"
    echo "-u     Update system"
    echo "-i     Reinstall all packages"
    echo "-a     Reinstall all foreign packages (using yaourt)"
    echo "-s     Sanity-check package lists before reinstalling using \$EDITOR (no effect if neither -i nor -a are given)"
    echo "-o     Run pacman-optimize after reinstallations (no effect if neither -i nor -a are given)"
    echo
    echo "Warning: All package installations are forced."
}

function fixmirrors()
{
    echo "Fixing pacman mirrorlist..."

    if [[ -d /etc/pacman.d ]]; then
	sudo rm -f /etc/pacman.d/mirrorlist
    else
	sudo mkdir -p /etc/pacman.d/
    fi

    sudo zsh -c "echo \"Server = ftp://ftp.archlinux.org/\\\$repo/os/`uname -m`\" > /etc/pacman.d/mirrorlist"
}

function fixpacman()
{
    echo "Manually reinstalling pacman..."

    wget ftp://ftp.archlinux.org/core/os/`uname -m`/core.db.tar.gz -O /tmp/core.db.tar.gz
    pacmanver=`tar tzf /tmp/core.db.tar.gz | grep pacman | head -n1 | sed "s/\///"`

    wget ftp://ftp.archlinux.org/core/os/`uname -m`/$pacmanver-`uname -m`.pkg.tar.gz -O /tmp/pacman.pkg.tar.gz
    sudo tar xvfz /tmp/pacman.pkg.tar.gz /
    sudo pacman -Sy pacman
}

function updatesystem()
{
    sudo pacman -Syuf
}

function repackages()
{
    echo "Generating package lists..."
    pacman -Qqe  | sort > /tmp/packages-qqe
    pacman -Qqem | sort > /tmp/packages-qqem
    pacman -Qqd  | sort > /tmp/packages-qqd
    pacman -Qqdm | sort > /tmp/packages-qqdm
    
    comm -23 /tmp/packages-qqe /tmp/packages-qqem > /tmp/packages1
    comm -23 /tmp/packages-qqd /tmp/packages-qqdm > /tmp/packages2
    
    mv /tmp/packages1 /tmp/packages-qqe
    mv /tmp/packages2 /tmp/packages-qqd
    
    if $3; then
	EDITOR=`[[ $EDITOR == "" ]] && echo "vim" || echo $EDITOR` # As much as I dislike it, vim *is* the one editor guaranteed to be everywhere...
	
	$EDITOR /tmp/packages-qqe
	$EDITOR /tmp/packages-qqd
	$EDITOR /tmp/packages-qqem
	$EDITOR /tmp/packages-qqdm
    fi
    
    sudo pacman -Syy
    
    if $1; then
	sudo pacman -Sf `cat /tmp/packages-qqe`
	sudo pacman -Sf --asdeps `cat /tmp/packages-qqd`
    fi
    
    if $2; then
	yaourt -Sf `cat /tmp/packages-qqem`
	yaourt -Sf --asdeps `cat /tmp/packages-qqdm`
    fi

    if $4; then
	sudo pacman-optimize
    fi
}

installpackages=false
installaur=false
sanitycheck=false
optimize=false

for arg in "${*[@]}"; do
    case $arg in
	"-h")
	    showhelp;;
        "-m")
            fixmirrors;;
	"-p")
	    fixpacman;;
	"-u")
	    updatesystem;;
	"-i")
	    installpackages=true;;
	"-a")
	    installaur=true;;
        "-s")
            sanitycheck=true;;
	"-o")
	    optimize=true;;
    esac
done

if $installpackages || $installaur; then
    repackages $installpackages $installaur $sanitycheck $optimize
fi
