#!/bin/sh
xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

~/.guix-home/profile/libexec/polkit-gnome-authentication-agent-1 &
xdg-user-dirs &
redshift &

xset s 300
xset dpms 300 300 300

XSECURELOCK_FONT='monospace' XSECURELOCK_PASSWORD_PROMPT=asterisks XSECURELOCK_SHOW_DATETIME=1 XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0 xss-lock -l -- xsecurelock &

exec dbus-launch --exit-with-session emacs
