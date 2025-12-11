#!/bin/sh

xrdb -merge ~/.Xresources

# P3 -> sRGB
xrandr --output eDP-1 --set CTM 0.8919,0.0809,0.0292,-0.0006,0.9885,0.0113,0.0000,0.0076,0.9925

export FREETYPE_PROPERTIES="cff:no-stem-darkening=0 autofitter:warping=0 truetype:interpreter-version=40"

xsettingsd &
dbus-update-activation-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP

exec emacs
