#!/bin/sh

# so the bus socket comes out connectable by whatever uid is on the other end of the bind mount
umask 000

# EXTERNAL auth resolves the connecting uid through NSS, so it needs an /etc/passwd entry to succeed
if [ -n "$TEST_UID" ]; then
  echo "tester:x:$TEST_UID:${TEST_GID:-$TEST_UID}::/tmp:/bin/sh" >> /etc/passwd
fi

exec dbus-daemon --nopidfile --nosyslog --nofork --print-address \
  --config-file=/usr/share/dbus-1/custom.conf
