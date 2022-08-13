#!/bin/bash

# outline from https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
# assumes install in 20.04 focal

mkdir -p ~/code/upstream
pushd ~/code/upstream

if ! test -d emacs; then
    git clone https://git.savannah.gnu.org/git/emacs.git
fi

pushd emacs

git checkout master
git pull --rebase

focal=0
if grep -c focal /etc/lsb-release; then
    focal=1
    export CC="gcc-10"
fi

# alternative use versions from add-apt-repository ppa:ubuntu-toolchain-r/ppa

if [[ $focal == 1 ]]; then
    sudo apt install -y gcc-10 libgccjit0 libgccjit-10-dev
else
    sudo apt install -y libgccjit0 libgccjit-11-dev
fi

sudo apt install -y libjansson4 libjansson-dev
sudo apt build-dep -y emacs-snapshot

JOBS=$(($(grep -c processor /proc/cpuinfo) - 2))
JOBS=$((JOBS>2 ? JOBS : 2))

if [[ "$1" == '--clean' ]]; then
    make clean extraclean distclean
fi

# Flags trimmed from ppa/emacs-snapshot `system-configuration-options` variable
# TODO: remove debug & try -O3 ?
./autogen.sh &&
    ./configure --build=x86_64-linux-gnu \
     --prefix="$HOME/usr" --program-transform-name='s/^emacs/gccmacs/' \
     --disable-silent-rules --with-modules=yes \
     --with-native-compilation --with-pgtk --with-mailutils --with-xwidgets \
     'CFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security' \
     'CPPFLAGS=-Wdate-time -D_FORTIFY_SOURCE=2' \
     'LDFLAGS=-Wl,-Bsymbolic-functions -Wl,-z,relro'

if [[ $focal == 1 ]]; then
    /usr/bin/time make -j "$JOBS" &&
        /usr/bin/time make install prefix="$HOME/usr" infodir="$HOME/usr/share/info"
else
    /usr/bin/time make -j "$JOBS" &&
        /usr/bin/time make install
fi

ln -sfv ~/usr/bin/gccmacs ~/usr/bin/emacs
ln -sfv ~/usr/bin/gccmacsclient ~/usr/bin/emacsclient
