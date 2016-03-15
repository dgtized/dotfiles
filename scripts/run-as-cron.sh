#!/bin/sh

# add
# * * * * * /usr/bin/env > /tmp/cron-env
# and then run with cron-env as first arg for same environment

. "$1"
exec /usr/bin/env -i "$SHELL" -c ". $1; $2"
