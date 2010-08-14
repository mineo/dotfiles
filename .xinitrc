xmodmap .Xmodmap &
thunar --daemon &
nitrogen --restore &
urxvtd -q -o -f
#trayfreq &
#tint2 &
killall mpdscribble
mpdscribble &
mpdnotify -s &
xscreensaver -no-splash &
pidgin &
#exec ck-launch-session openbox-session
exec ck-launch-session i3 --force-xinerama
