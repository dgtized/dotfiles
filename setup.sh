#!/bin/bash

# wget -r -Imain/home-config http://svn.dgtized.net/main/

function main () {
    echo "* running setup.sh from $DOTC_DIR for $DOTC_NAME"
    
    echo "export DOTC_DIR=~/$DOTC_DIR" > site-config
    echo "export DOTC_NAME=$DOTC_NAME" >> site-config

    ln -sfv ${DOTC_DIR}/site-config ~/.site-config
    
    ln -sfv ${DOTC_DIR}/bashrc ~/.bashrc
    if [[ -e ${DOTC_DIR}/emacs-${DOTC_NAME} ]]; then
    	ln -sfv ${DOTC_DIR}/emacs-${DOTC_NAME} ~/.emacs
    else
	ln -sfv ~/${DOTC_DIR}/emacs ~/.emacs
    fi

    ln -sfv ${DOTC_DIR}/vimrc ~/.vimrc
    
    ln -sfv ${DOTC_DIR}/irbrc ~/.irbrc
    ln -sfv ${DOTC_DIR}/toprc ~/.toprc
    ln -sfv ${DOTC_DIR}/inputrc ~/.inputrc
    ln -sfv ${DOTC_DIR}/screenrc ~/.screenrc
    if [[ -d ~/.subversion ]]; then
	    ln -sfv ../${DOTC_DIR}/svn-config ~/.subversion/config
    fi

    ln -sfv ${DOTC_DIR}/pinerc ~/.pinerc
    ln -sfv ${DOTC_DIR}/muttrc ~/.muttrc
    
    if [[ $DOTC_NAME == "cec" || $DOTC_NAME == "cse" ]]; then
	ln -sfv ${DOTC_DIR}/cshrc.mine-${DOTC_NAME} ~/.cshrc.mine
    fi
    
    # ssh related config
    mkdir -pv -m 700 $HOME/.ssh
    chmod 600 ${HOME}/${DOTC_DIR}/ssh/*
    rm -r ~/.ssh/{authorized_keys,config}
    cp -fv ../${DOTC_DIR}/ssh/authorized_keys ~/.ssh/authorized_keys
    cp -fv ../${DOTC_DIR}/ssh/config ~/.ssh/config
    chmod 600 ${HOME}/.ssh/{authorized_keys,config}

    # These happen hear so they happen after setup.sh reload
    svn export http://svn.collab.net/repos/svn/trunk/contrib/client-side/emacs/psvn.el site-lisp/psvn.el
    svn export http://svn.clouder.jp/repos/public/yaml-mode/trunk/yaml-mode.el site-lisp/yaml-mode.el
    rm -rf site-lisp/ruby && svn export http://svn.ruby-lang.org/repos/ruby/trunk/misc site-lisp/ruby
    echo "Compiling site-lisp... (see site-lisp/compile.log for detail)"
    (emacs -L site-lisp -batch -f batch-byte-compile \
	site-lisp/*.el site-lisp/*/*.el ~/.emacs 2>&1) > site-lisp/compile.log
    
    mkdir -pv $HOME/.bashist

    # lets get some bash completion if we don't have it
    if [[ ! -f /etc/bash_completion ]]; then
	if wget -N "http://www.caliban.org/files/bash/bash-completion-latest.tar.gz"; then
	    tar xzf bash-completion-latest.tar.gz    
	fi
    fi    
    mkdir -pv $HOME/usr/bin
    for script in `find scripts -type f | grep -v .svn`; do
	ln -sfv ../../${DOTC_DIR}/$script ~/usr/bin;
    done
    if [[ -d $HOME/.scripts/ ]]; then
    	for script in `find ${HOME}/.scripts -type f | grep -v .svn`; do
		ln -sfv ../../$script ~/usr/bin;
	done
    fi
    # this would be cool but then it forgets where it's from
    #ln -sfv ${DOTC_DIR}/setup.sh ~/usr/bin/home-config.sh
}

function valid_name () {
    until [[ $DOTC_NAME == "bio" ||
	     $DOTC_NAME == "gentoo" ||
	     $DOTC_NAME == "debian" ||
	     $DOTC_NAME == "cse" ||
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
    elif [ -f /etc/debian_version ]; then
	DOTC_NAME="debian"
    else
	echo "Usage: setup.sh config-name"
	exit
    fi
else
    DOTC_NAME=$1
fi

pushd $(dirname $0) > /dev/null
DIRNAME=`pwd`/setup.sh
popd > /dev/null
DOTC_DIR=`dirname ${DIRNAME//$HOME/} | sed 's/^\///'`
unset DIRNAME

if [[ -d $HOME/$DOTC_DIR ]]; then
    cd $HOME/$DOTC_DIR

    if [[ $1 == "clean" ]]; then
	echo "Cleaning..."
	rm -rfv bash-completion-latest.tar.gz bash_completion site-config \
	    *~ *\# site-lisp/*.elc site-lisp/compile.log
	exit
    elif [[ $1 == "up" || $1 == "update" ]]; then
        echo "Updating Configuration..."

	branch=`git branch | grep '*' | sed -e 's/\* //g'`
	echo " * saving local changes to stash"
	git stash save
	if [[ $branch != "master" ]]; then
	    echo " * switching from $branch to master"
	    git checkout master
	fi	
	echo " * pulling remote"
	git pull
	if [[ $branch != "master" ]]; then
	    echo " * returning to $branch"
	    git checkout $branch
	fi
	echo " * reapplying local changes"
	git stash apply
	unset branch
        
	test -e site-config && source site-config
	valid_name
	echo "Reloading setup.sh in case of remote change"
	exec ./setup.sh ${DOTC_NAME}
    fi

    valid_name
    source color-bash    

    main # now that we know everything will get cleaned up

    echo 
    echo "Site Config for `hostname`-${DOTC_NAME}:"
    cat site-config
else
    echo "Can't find $DOTC_DIR"
fi
