#!/bin/bash

function main () {
    echo "* running setup.sh from $DOTC_DIR"

    cat <<-EOF > $DOTC_CONFIG
export DOTC_DIR=$DOTC_DIR
EOF

    for dotdir in `find $DOTC_DIR/dot/ -type d`; do
        mkdir -pv $HOME/.${dotdir#*dot/}
    done
    mkdir -pv $HOME/.bashist $HOME/usr/bin $HOME/gocode

    for dot in `find $DOTC_DIR/dot -type f`; do
        ln -sfv $dot $HOME/.${dot#*dot/}
    done

    # setup fonts for emacs correctly
    if which xrdb > /dev/null; then
        xrdb -merge $HOME/.Xresources
    fi

    # ssh related config
    mkdir -pv -m 700 $HOME/.ssh/control
    touch ~/.ssh/authorized_keys
    cat ${DOTC_DIR}/sshconfig.d/*.conf > $HOME/.ssh/config
    echo "Updated ~/.ssh/config"
    chmod 600 $HOME/.ssh/{authorized_keys,config}

    for script in `find ${DOTC_DIR}/scripts -type f`; do
        ln -sfv $script $HOME/usr/bin;
    done
    # this would be cool but then it forgets where it's from
    #ln -sfv ${DOTC_DIR}/setup.sh ~/usr/bin/home-config.sh
}

function slow_updates () {
    # These happen here so they happen after setup.sh reload

    PKG=gnome-terminal-colors-solarized
    if [ ! -d $DOTC_DIR/$PKG ]; then
        git clone https://github.com/Anthony25/$PKG.git $DOTC_DIR/$PKG
    fi

    setup_emacs
    if [[ -e /usr/bin/xmonad ]]; then
        xmonad --recompile
    fi

    if [ ! -e ~/usr/plantuml.jar ]; then
        echo "install plantuml"
        wget https://github.com/plantuml/plantuml/releases/download/v1.2023.5/plantuml.jar -O ~/usr/plantuml.jar
    fi
}

function setup_emacs () {
    if [ ! -d $DOTC_DIR/cask ]; then
        git clone git@github.com:cask/cask.git $DOTC_DIR/cask
    fi

    mkdir -p ~/.emacs.d
    ln -sfv $DOTC_DIR/site-lisp/Cask ~/.emacs.d/Cask
    ln -sfv $DOTC_DIR/site-lisp/init.el ~/.emacs.d/init.el

    pushd ~/.emacs.d
    echo "Initializing Cask ... (see .emacs.d/cask.log)"
    ($DOTC_DIR/cask/bin/cask 2>&1) > cask.log
    popd

    pushd site-lisp
    echo "Compiling site-lisp... (see site-lisp/compile.log)"
    (emacs -L . -Q -batch -f batch-byte-compile 2>&1) > compile.log
    popd
}

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

        branch=`git rev-parse --abbrev-ref HEAD`
        dirty=`git status --porcelain`

        if [[ $dirty ]]; then
            echo " * saving local changes to stash"
            git stash save
        fi

        if [[ $branch != "master" ]]; then
            echo " * switching from $branch to master"
            git checkout master
        fi
        echo " * pulling remote"
        git pull --rebase
        if [[ $branch != "master" ]]; then
            echo " * returning to $branch"
            git checkout $branch
        fi

        if [[ $dirty ]]; then
            echo " * reapplying local changes"
            git stash pop
        fi

        unset branch

        test -e $DOTC_CONFIG && source $DOTC_CONFIG
        echo "Reloading setup.sh in case of remote change"
        exec ./setup.sh
    fi

    source color-bash

    main # now that we know everything will get cleaned up

    if [[ $1 != "fast" ]]; then
        slow_updates
    fi

    echo
    echo "Site Config for `hostname`:"
    cat $DOTC_CONFIG
else
    echo "Can't find $DOTC_DIR"
fi
