#!/bin/bash

# call from gnome-session-properties as $HOME/usr/bin/gnome-xinit.sh
# to script applications to launch at initial login

xrdb -merge $HOME/.Xresources
source "$HOME/.bashrc"

# initial apps
uxterm -e "tmux -2 new-session -A -s default" &
"$HOME/usr/bin/ec" -c &

if test -e /usr/bin/google-chrome-beta ; then
    google-chrome-beta &
else
    google-chrome-stable &
fi

slack &

