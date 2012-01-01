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
#if [ $(hostname) = "slartibartfass" ]; then
    #if [ $2 == "nomulti" ];
            #then disper -S && disper -e -d DFP-0
    #else
        #echo "foo" > bar
        #disper -S && disper -e -d DFP-0,DFP-1
    #fi
#fi
~/dev/bin/xdisplay.sh
exec ck-launch-session bash -c "udiskie & i3" # --force-xinerama"
