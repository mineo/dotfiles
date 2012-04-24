xmodmap .Xmodmap &
urxvtd -q -o -f
#trayfreq &
#tint2 &
#mpdnotify -s &
killall mpdscribble
mpdscribble
dropboxd &
fehlstart &
xflux -l 50 -g 10 > /dev/null
#exec ck-launch-session openbox-session
#exec ck-launch-session i3 --force-xinerama
if [ $(hostname) = "slartibartfass" ]; then
    if [ $2 == "nomulti" ];
            then disper -S && disper -e -d DFP-0
    else
        disper -S && disper -e -d DFP-0,DFP-1
    fi
fi
nitrogen --restore &
#~/dev/bin/xdisplay.sh
exec ck-launch-session bash -c "udiskie & i3 --force-xinerama"
