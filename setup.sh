#!/bin/bash

function main () {
    echo "* running setup.sh from $DOTC_DIR for $DOTC_NAME"

    cat <<-EOF > $DOTC_CONFIG
export DOTC_DIR=$DOTC_DIR
export DOTC_NAME=$DOTC_NAME
EOF

    for dot in `find $DOTC_DIR/dot -type f`; do
        ln -sfv $dot $HOME/.`basename $dot`
    done

    if [[ -d ~/.subversion ]]; then
        ln -sfv ${DOTC_DIR}/svn-config ~/.subversion/config
    fi

    # setup fonts for emacs correctly
    if which xrdb > /dev/null; then
        xrdb -merge $HOME/.Xresources
    fi

    # ssh related config
    mkdir -pv -m 700 $HOME/.ssh
    touch ~/.ssh/{authorized_keys,config}
    chmod 600 ${HOME}/.ssh/{authorized_keys,config}

    # These happen here so they happen after setup.sh reload
    git submodule update --init

    pushd site-lisp
    setup_emacs
    popd

    mkdir -pv $HOME/.bashist

    mkdir -pv $HOME/usr/bin
    for script in `find ${DOTC_DIR}/scripts -type f`; do
        ln -sfv $script $HOME/usr/bin;
    done
    # this would be cool but then it forgets where it's from
    #ln -sfv ${DOTC_DIR}/setup.sh ~/usr/bin/home-config.sh
}

function setup_emacs () {
    echo "Compiling site-lisp... (see site-lisp/compile.log for detail)"
    (emacs -L . -L vendor/groovy -batch -f batch-byte-compile \
        *.el vendor/**/*.el 2>&1) > compile.log
}

function valid_name () {
    until [[ $DOTC_NAME == "bio" ||
             $DOTC_NAME == "gentoo" ||
             $DOTC_NAME == "debian" ||
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

: ${DOTC_DIR:=$HOME/.home-config}
: ${DOTC_CONFIG:=$DOTC_DIR/dot/site-config}

if [[ -d $DOTC_DIR ]]; then
    cd $DOTC_DIR

    if [[ $1 == "clean" ]]; then
        echo "Cleaning..."
        rm -rfv $DOTC_CONFIG *~ *\# site-lisp/*.elc site-lisp/compile.log
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
        git stash pop
        unset branch

        test -e $DOTC_CONFIG && source $DOTC_CONFIG
        valid_name
        echo "Reloading setup.sh in case of remote change"
        exec ./setup.sh ${DOTC_NAME}
    fi

    valid_name
    source color-bash

    main # now that we know everything will get cleaned up

    echo
    echo "Site Config for `hostname`-${DOTC_NAME}:"
    cat $DOTC_CONFIG
else
    echo "Can't find $DOTC_DIR"
fi
