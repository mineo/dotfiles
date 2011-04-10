xmodmap .Xmodmap &
nitrogen --restore &
urxvtd -q -o -f
#trayfreq &
#tint2 &
#mpdnotify -s &
killall mpdscribble
mpdscribble
dropboxd &
#exec ck-launch-session openbox-session
#exec ck-launch-session i3 --force-xinerama
exec ck-launch-session bash -c "udiskie & subtle -n"
