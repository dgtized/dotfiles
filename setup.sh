#!/bin/bash

function main () {
    echo "* running setup.sh from $DOTC_DIR for $DOTC_NAME"

    echo "export DOTC_DIR=$DOTC_DIR" > site-config
    echo "export DOTC_NAME=$DOTC_NAME" >> site-config
    ln -sfv ${DOTC_DIR}/site-config ~/.site-config
    
    ln -sfv ${DOTC_DIR}/bashrc ~/.bashrc
    if [[ -e ${DOTC_DIR}/emacs-${DOTC_NAME} ]]; then
    	ln -sfv ${DOTC_DIR}/emacs-${DOTC_NAME} ~/.emacs
    else
	ln -sfv ${DOTC_DIR}/emacs ~/.emacs
    fi

    ln -sfv ${DOTC_DIR}/vimrc ~/.vimrc
    
    ln -sfv ${DOTC_DIR}/irbrc ~/.irbrc
    ln -sfv ${DOTC_DIR}/toprc ~/.toprc
    ln -sfv ${DOTC_DIR}/inputrc ~/.inputrc
    ln -sfv ${DOTC_DIR}/screenrc ~/.screenrc

    ln -sfv ${DOTC_DIR}/pinerc ~/.pinerc
    ln -sfv ${DOTC_DIR}/muttrc ~/.muttrc
    
    if [[ $DOTC_NAME == "cec" ]]; then
	ln -sfv ${DOTC_DIR}/cshrc.mine ~/.cshrc.mine
    fi
    
    # ssh related config
    mkdir -pv ~/.ssh
    ln -sfv ${DOTC_DIR}/ssh/authorized_keys ~/.ssh/authorized_keys
    ln -sfv ${DOTC_DIR}/ssh/config ~/.ssh/config
    chmod -v 700 ~/.ssh 

    echo "Compiling site-lisp... (see site-lisp/compile.log for detail)"
    (emacs -l ~/.emacs -batch -f batch-byte-compile \
	${DOTC_DIR}/site-lisp/*.el ~/.emacs 2>&1) > \
        ${DOTC_DIR}/site-lisp/compile.log
    
    # lets get some bash completion if we don't have it
    if [[ $DOTC_NAME != "gentoo" ]]; then
	if wget -N http://www.caliban.org/files/bash/bash-completion-latest.tar.gz; then
	    tar xzf bash-completion-latest.tar.gz    
	fi
    fi    
}

function valid_name () {
    until [[ $DOTC_NAME == "bio" ||
	     $DOTC_NAME == "gentoo" ||
	     $DOTC_NAME == "cec" ||
	     $DOTC_NAME == "dreamhost" ]]; do
	echo "DOTC_NAME cannot be [$DOTC_NAME], what is it? "
	read -e var
	eval "DOTC_NAME=\$var;"
    done
}

if [[ $# -ne 1 ]]; then
    if [ -f /etc/gentoo-release ]; then
	DOTC_NAME="gentoo"
    else
	echo "Usage: setup.sh config-name"
	exit
    fi
else
    DOTC_NAME=$1
fi

DOTC_DIR=`dirname $0`

if [[ -d $DOTC_DIR ]]; then
    pushd . > /dev/null 2>&1
    cd $DOTC_DIR

    if [[ $1 == "clean" ]]; then
	echo "Cleaning..."
	rm -rfv bash-completion-latest.tar.gz bash_completion site-config \
	    *~ *\# site-lisp/*.elc site-lisp/compile.log
	exit
    elif [[ $1 == "up" || $1 == "update" ]]; then
        echo "Updating Configuration..."
        svn update
        test -e site-config && source site-config
	valid_name
	echo "Reloading setup.sh in case of remote change"
	exec ${DOTC_DIR}/setup.sh $DOTC_NAME
    fi

    valid_name
    source color-bash    

    main # now that we know everything will get cleaned up

    echo 
    echo "Site Config for `hostname`-${DOTC_NAME}:"
    cat site-config

    popd > /dev/null 2>&1    
fi
