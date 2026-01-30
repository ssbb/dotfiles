#!/bin/sh
xrdb -merge ~/.Xresources

~/.guix-home/profile/libexec/polkit-gnome-authentication-agent-1 &
xdg-user-dirs &
redshift &

exec dbus-launch --exit-with-session emacs --maximized --fullscreen --no-splash
