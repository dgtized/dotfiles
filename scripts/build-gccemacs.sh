#!/usr/bin/bash

# outline from https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
# assumes install in 20.04 focal

mkdir -p ~/code/upstream
pushd ~/code/upstream

if ! test -d emacs; then
    git clone https://git.savannah.gnu.org/git/emacs.git
    git checkout -b native-comp origin/feature/native-comp
fi

pushd emacs

# alternative use versions from add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt install -y gcc-10 libgccjit0 libgccjit-10-dev
sudo apt install -y libjansson4 libjansson-dev
sudo apt build-dep -y emacs-snapshot

CC="gcc-10"
JOBS=$(($(grep -c processor /proc/cpuinfo) - 2))
JOBS=$((JOBS>2 ? JOBS : 2))

./autogen.sh

# Flags trimmed from ppa/emacs-snapshot `system-configuration-options` variable
# TODO: remove debug & try -O3 ?
./autogen.sh &&
    ./configure --build=x86_64-linux-gnu \
     --prefix="$HOME/usr" --program-transform-name='s/^emacs/gmacs/' \
     --disable-silent-rules --disable-maintainer-mode --disable-dependency-tracking \
     --with-modules=yes --with-x=yes --with-x-toolkit=gtk3 --with-xwidgets=yes \
     --with-nativecomp --with-mailutils \
     'CFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security' \
     'CPPFLAGS=-Wdate-time -D_FORTIFY_SOURCE=2' \
     'LDFLAGS=-Wl,-Bsymbolic-functions -Wl,-z,relro'

make -j "$JOBS" && make install
