#!/bin/sh
xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

~/.guix-home/profile/libexec/polkit-gnome-authentication-agent-1 &
xdg-user-dirs &
redshift &

# Disable screensaver and DPMS - we manage locking ourselves
xset s off -dpms

# Idle 5min: xautolock →  locker-guard (inhibitor check) →  xsecurelock
# Idle 15min: xautolock →  loginctl suspend
# loginctl lock-session: xss-lock →  xautolock -locknow →  xsecurelock
# Lid close / suspend: xss-lock →  xautolock -locknow →  xsecurelock
XSECURELOCK_FONT='monospace' \
XSECURELOCK_PASSWORD_PROMPT=asterisks \
XSECURELOCK_SHOW_DATETIME=1 \
XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0 \
XSECURELOCK_BLANK_TIMEOUT=0 \
XSECURELOCK_BLANK_DPMS_STATE=off \
xautolock \
    -detectsleep \
    -noclose \
    -locker "locker-guard xsecurelock" \
    -nowlocker xsecurelock \
    -killer "loginctl suspend" \
    -time 5 -killtime 10 2>&1 | logger -t xautolock &

# xss-lock used as a bridge between dbus and xautolock
xss-lock -l -- xautolock -locknow &

exec dbus-launch --exit-with-session emacs
