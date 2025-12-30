#!/bin/sh
xrdb -merge ~/.Xresources

xsettingsd &
xdg-user-dirs &
redshift &

exec dbus-launch --exit-with-session emacs
