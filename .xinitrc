xmodmap .Xmodmap &
nitrogen --restore &
urxvtd -q -o -f
#trayfreq &
#tint2 &
mpdnotify -s &
killall mpdscribble
mpdscribble
dropboxd &
#exec ck-launch-session openbox-session
exec i3 --force-xinerama
