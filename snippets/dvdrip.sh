#!/bin/bash

#mplayer dvd://1 -vf cropdetect

#mencoder dvd://7 -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
#mencoder dvd://7 -oac copy -ovc xvid -xvidencopts pass=2:bitrate=800 -o season1- extras.avi

# mencoder dvd://7 -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=720:480:0:0 -o season1-extras-2.avi

# dvd://1 -vf crop=704:480:10:0
mencoder dvd://1 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://1 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=704:480:10:0 -o s1e1.avi
rm -f dvix2pass.log frameno.avi
# dvd://2 -vf crop=720:480:0:0
mencoder dvd://2 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://2 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=720:480:0:0 -o s1e2.avi
rm -f dvix2pass.log frameno.avi
# dvd://3 -vf crop=720:480:0:0
mencoder dvd://3 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://3 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=720:480:0:0 -o s1e3.avi
rm -f dvix2pass.log frameno.avi
# dvd://4 -vf crop=704:464:10:10
mencoder dvd://4 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://4 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=704:464:10:10 -o s1e4.avi
rm -f dvix2pass.log frameno.avi
# dvd://5 -vf crop=704:480:10:0
mencoder dvd://5 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://5 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=704:480:10:0 -o s1e5.avi
rm -f dvix2pass.log frameno.avi
# dvd://6 -vf crop=720:480:0:0
mencoder dvd://6 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://6 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=720:480:0:0 -o s1e6.avi
rm -f dvix2pass.log frameno.avi
# dvd://7 -vf crop=720:480:0:0
mencoder dvd://7 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=1 -o /dev/null
mencoder dvd://7 -alang English -sid 0 -vobsubout /data/the_office -oac copy -ovc xvid -xvidencopts pass=2:bitrate=1000 -vf crop=720:480:0:0 -o s1-extra.avi
rm -f dvix2pass.log frameno.avi
