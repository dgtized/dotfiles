#!/bin/bash

# From https://medium.com/@petehouston/play-webcam-using-mplayer-fb4b8729ff88
mplayer tv:// -tv driver=v4l2:device=/dev/video0:width=1280:height=720:fps=30:outfmt=yuy2
