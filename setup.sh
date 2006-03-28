#!/bin/bash

if [[ $BASH_ARGC != 1 ]]; then
    if [ -f /etc/gentoo-release ]; then
	CONFIG_NAME="gentoo"
    else
	echo "Usage: setup.sh config-name"
	exit
    fi
else
    if [[ $1 == "clean" ]]; then
	rm -rfv bash-completion-latest.tar.gz bash-completion *~	
	exit
    fi
    CONFIG_NAME=$1
fi

CONFIG_DIR=`dirname $0`

if [[ -d $CONFIG_DIR ]]; then
    pushd . > /dev/null 2>&1
    cd $CONFIG_DIR

    echo "CONFIG_DIR=$CONFIG_DIR" > site-config
    echo "CONFIG_NAME=$CONFIG_NAME" >> site-config
    
    echo "* running setup.sh from $CONFIG_DIR"
    
    source $CONFIG_DIR/color-bash
    
    ln -sfv ${CONFIG_DIR}/bashrc ~/.bashrc
    ln -sfv ${CONFIG_DIR}/emacs-${CONFIG_NAME} ~/.emacs
    ln -sfv ${CONFIG_DIR}/vimrc ~/.vimrc
    
    ln -sfv ${CONFIG_DIR}/irbrc ~/.irbrc
    ln -sfv ${CONFIG_DIR}/inputrc ~/.inputrc
    ln -sfv ${CONFIG_DIR}/screenrc ~/.screenrc
    
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

    echo 
    echo "Site Config for `hostname`-$1:"
    cat site-config
    
    popd > /dev/null 2>&1
fi
