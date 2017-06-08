#!/bin/bash

# FIXME: changing this with xrandr isn't sticky enough. After the monitor wakes
# back up from sleep it keeps reverting to 1920x1080 until the setting is
# adjusted with unity-control-center display settings again
XRANDR=/tmp/xrandr.toggle-monitor
xrandr | grep DP-0 > $XRANDR
if [[ $? == 1 ]]; then
    echo "DP-0 not found"
    exit 1
fi

if grep 3840x2160 $XRANDR; then
    xrandr --output DP-0 --mode 1920x1080 --rate 60
else
    xrandr --output DP-0 --mode 3840x2160 --rate 60
fi
