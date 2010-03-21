#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

  /usr/bin/xmodmap /home/wieland/.Xmodmap&
urxvtd -q -o -f &
nitrogen --restore &
#  exec wmii
tint2 &
#trayfreq &
netcfg-tray &
thunar --daemon &
exec ck-launch-session openbox-session
#exec ck-launch-session wmii
