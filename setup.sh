#!/bin/bash

CONFIG_DIR="$HOME/clgc-config"
CONFIG_NAME="gentoo"

ln -sfv ${CONFIG_DIR}/bashrc ~/.bashrc
ln -sfv ${CONFIG_DIR}/emacs-${CONFIG_NAME} ~/.emacs
ln -sfv ${CONFIG_DIR}/vimrc ~/.vimrc

ln -sfv ${CONFIG_DIR}/irbrc ~/.irbrc
ln -sfv ${CONFIG_DIR}/screenrc ~/.screenrc

mkdir -vp ~/.ssh
ln -sfv ${CONFIG_DIR}/ssh/authorized_keys ~/.ssh/authorized_keys
ln -sfv ${CONFIG_DIR}/ssh/config ~/.ssh/config
