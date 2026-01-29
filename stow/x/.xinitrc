#!/bin/sh
xrdb -merge ~/.Xresources

xdg-user-dirs &
redshift &

exec dbus-launch --exit-with-session emacs --maximized --fullscreen --no-splash
