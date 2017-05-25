#!/bin/bash

# Usage: long_running_task; slack-msg.sh @charles "Task Completed with $?"
CHANNEL=${1:?Provide channel} # @username for self notify, otherwise channel
MSG=${2:?Provide message}

cat <<EOF | envsubst | curl -X POST -H 'Content-type: application/json' $SLACK_WEBHOOK --data @-
{
  "username": "foobarbaz",
  "channel": "$CHANNEL",
  "text": "$MSG"
}
EOF


