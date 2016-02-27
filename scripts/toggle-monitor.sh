#!/bin/bash

# FIXME: changing this with xrandr isn't sticky enough. After the monitor wakes
# back up from sleep it keeps reverting to 1920x1080 until the setting is
# adjusted with unity-control-center display settings again
if xrandr | grep DP-0 | grep 3840x2160; then
    xrandr --output DP-0 --mode 1920x1080 --rate 60
else
    xrandr --output DP-0 --mode 3840x2160 --rate 60
fi
