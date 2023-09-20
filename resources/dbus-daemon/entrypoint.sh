#!/bin/sh

readonly dbusPort=1234
readonly internalPort="$((dbusPort + 1))"

dbus-daemon --config-file=/etc/dbus-1/custom.conf --nopidfile --nosyslog --fork \
  --address=tcp:host=localhost,port="$internalPort",family=ipv4,bind='*'

exec socat -v -d -d TCP-LISTEN:"$dbusPort",reuseaddr,fork TCP:localhost:"$internalPort"
