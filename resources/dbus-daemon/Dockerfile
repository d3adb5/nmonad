FROM alpine:3.18.3@sha256:c5c5fda71656f28e49ac9c5416b3643eaa6a108a8093151d6d1afc9463be8e33

RUN apk add --no-cache dbus=1.14.8-r0 socat=1.7.4.4-r1

COPY ./dbus.conf /etc/dbus-1/custom.conf
COPY ./entrypoint.sh /opt/entrypoint.sh

ENV DBUS_VERBOSE=1
EXPOSE 1234

ENTRYPOINT [ "/opt/entrypoint.sh" ]
