FROM alpine:3.24.1@sha256:28bd5fe8b56d1bd048e5babf5b10710ebe0bae67db86916198a6eec434943f8b

RUN apk add --no-cache dbus=1.16.2-r2

COPY ./dbus.conf /usr/share/dbus-1/custom.conf
COPY ./entrypoint.sh /opt/entrypoint.sh

ENV DBUS_VERBOSE=1

ENTRYPOINT [ "/bin/sh", "/opt/entrypoint.sh" ]
