#!/bin/bash

if [[ $BASH_ARGC != 1 ]]; then
    if [ -f /etc/gentoo-release ]; then
	CONFIG_NAME="gentoo"
    else
	echo "Usage: setup.sh config-name"
	exit
    fi
else
    CONFIG_NAME=$1
fi

CONFIG_DIR=`dirname $0`

function main () {
    echo "* running setup.sh from $CONFIG_DIR"

    echo "CONFIG_DIR=$CONFIG_DIR" > site-config
    echo "CONFIG_NAME=$CONFIG_NAME" >> site-config
    
    ln -sfv bashrc ~/.bashrc
    ln -sfv emacs-${CONFIG_NAME} ~/.emacs
    ln -sfv vimrc ~/.vimrc
    
    ln -sfv irbrc ~/.irbrc
    ln -sfv inputrc ~/.inputrc
    ln -sfv screenrc ~/.screenrc

    if [[ $CONFIG_NAME == "cec" ]]; then
	ln -sfv .cshrc.mine cshrc.mine
	ln -sfv pinerc .pinerc
    fi
    
    # ssh related config
    mkdir -vp ~/.ssh
    ln -sfv ${CONFIG_DIR}/ssh/authorized_keys ~/.ssh/authorized_keys
    ln -sfv ${CONFIG_DIR}/ssh/config ~/.ssh/config
    
    # lets get some bash completion if we don't have it
    if [[ $CONFIG_NAME != "gentoo" ]]; then
	if wget -N http://www.caliban.org/files/bash/bash-completion-latest.tar.gz; then
	    tar xzf bash-completion-latest.tar.gz    
	fi
    fi    
}

if [[ -d $CONFIG_DIR ]]; then
    pushd . > /dev/null 2>&1
    cd $CONFIG_DIR

    if [[ $1 == "clean" ]]; then
	echo "Cleaning..."
	rm -rfv bash-completion-latest.tar.gz bash_completion site-config *~ *#	
	popd > /dev/null 2>&1	
	exit
    fi

    source color-bash    

    main # now that we know everything will get cleaned up

    echo 
    echo "Site Config for `hostname`-$1:"
    cat site-config

    popd > /dev/null 2>&1    
fi
