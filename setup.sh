#!/bin/bash

CONFIG_DIR=`dirname $0`
CONFIG_NAME="gentoo"

echo "* running setup.sh from $CONFIG_DIR"

source $CONFIG_DIR/color-bash

ln -sfv ${CONFIG_DIR}/bashrc ~/.bashrc
ln -sfv ${CONFIG_DIR}/emacs-${CONFIG_NAME} ~/.emacs
ln -sfv ${CONFIG_DIR}/vimrc ~/.vimrc

ln -sfv ${CONFIG_DIR}/irbrc ~/.irbrc
ln -sfv ${CONFIG_DIR}/screenrc ~/.screenrc

# ssh related config
mkdir -vp ~/.ssh
ln -sfv ${CONFIG_DIR}/ssh/authorized_keys ~/.ssh/authorized_keys
ln -sfv ${CONFIG_DIR}/ssh/config ~/.ssh/config
