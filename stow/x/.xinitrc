#!/bin/sh

xrdb -merge ~/.Xresources

xsettingsd &
xdg-user-dirs &

dbus-update-activation-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP

exec emacs
