#!/bin/bash

# outline from https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

mkdir -p ~/code/upstream
pushd ~/code/upstream

if ! test -d emacs; then
    git clone https://git.savannah.gnu.org/git/emacs.git
fi

pushd emacs

git checkout master
git pull --rebase

# alternative use versions from add-apt-repository ppa:ubuntu-toolchain-r/ppa

# libtool-bin is for vterm support
sudo apt install -y libgccjit0 libgccjit-14-dev libjansson4 libjansson-dev \
     libtool-bin libtree-sitter-dev \
     libgtk-3-dev libgnutls28-dev \
     libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo

sudo apt build-dep -y emacs-snapshot

JOBS=$(($(grep -c processor /proc/cpuinfo) - 2))
JOBS=$((JOBS>2 ? JOBS : 2))

if [[ "$1" == '--clean' ]]; then
    make clean extraclean distclean
fi

# Flags trimmed from ppa/emacs-snapshot `system-configuration-options` variable
# removed:  --with-xwidgets (context: https://www.reddit.com/r/emacs/comments/1fpd3dk/problem_compiling_latest_git_version/)
# TODO: remove debug & try -O3 ?
./autogen.sh &&
    ./configure --build=x86_64-linux-gnu \
     --prefix="$HOME/usr" --program-transform-name='s/^emacs/gccmacs/' \
     --disable-silent-rules --with-modules \
     --with-tree-sitter --with-native-compilation=aot --with-mailutils --with-no-xwidgets \
     'CFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security' \
     'CPPFLAGS=-Wdate-time -D_FORTIFY_SOURCE=2' \
     'LDFLAGS=-Wl,-Bsymbolic-functions -Wl,-z,relro'

/usr/bin/time make -j "$JOBS" bootstrap &&
    /usr/bin/time make -j "$JOBS"
    /usr/bin/time make install

ln -sfv ~/usr/bin/gccmacs ~/usr/bin/emacs
ln -sfv ~/usr/bin/gccmacsclient ~/usr/bin/emacsclient
