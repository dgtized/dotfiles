#!/bin/bash

if xrandr | grep DP-0 | grep 3840x2160; then
    xrandr --output DP-0 --mode 1920x1080 --rate 60
else
    xrandr --output DP-0 --mode 3840x2160 --rate 60
fi
