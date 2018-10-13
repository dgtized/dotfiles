#!/bin/bash

# call from gnome-session-properties as $HOME/usr/bin/gnome-xinit.sh
# to script applications to launch at initial login

xrdb -merge $HOME/.Xresources

uxterm -e "tmux -2 new-session -A -s default" &

google-chrome-beta &

source ~/.bashrc && $HOME/usr/bin/ec -c &
