#!/bin/sh
xrdb -merge ~/.Xresources

~/.guix-home/profile/libexec/polkit-gnome-authentication-agent-1 &
xdg-user-dirs &
redshift &

xset s 300

XSECURELOCK_FONT='monospace' XSECURELOCK_PASSWORD_PROMPT=asterisks XSECURELOCK_SHOW_DATETIME=1 XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0 xss-lock -l -- xsecurelock &

exec dbus-launch --exit-with-session emacs
