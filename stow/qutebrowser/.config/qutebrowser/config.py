config.load_autoconfig(False)

c.qt.args = [
    "ignore-gpu-blocklist",
    "enable-features=VaapiIgnoreDriverChecks,VaapiVideoDecodeLinuxGL"
]

c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

c.new_instance_open_target = "window"
c.new_instance_open_target_window = "last-visible"

c.tabs.tabs_are_windows = True
c.tabs.show = "never"

c.statusbar.show = "never"

c.fonts.default_family = "monospace"

c.hints.chars = "arstgneio"  # colemak dh

config.bind("o", "emacs '(qutebrowser-launcher)'")
config.bind("O", "emacs '(qutebrowser-launcher-tab)'")
config.bind("wo", "emacs '(qutebrowser-launcher-window)'")
config.bind("W", "emacs '(qutebrowser-launcher-private)'")
config.bind("go", "emacs '(qutebrowser-launcher \"{url:pretty}\")'")
config.bind("gO", "emacs '(qutebrowser-launcher-tab \"{url:pretty}\")'")

config.bind(',p', "emacs '(qutebrowser-pass \"{url}\")'")
config.bind(',P', "emacs '(qutebrowser-pass-password-only \"{url}\")'")
config.bind(',o', "emacs '(qutebrowser-pass-otp \"{url}\")'")

config.source("emacs_theme.py")
