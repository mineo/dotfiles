if [ $(hostname) = "slartibartfass" ]; then
    xrandr --output DVI-I-1 --mode 1440x900 --pos 0x0 --rotate normal --output DVI-I-0 --off --output HDMI-1 --mode 1600x1200 --pos 1440x0 --rotate left
fi

dropboxd &
gajim &
killall mpdscribble
mpdscribble
redshift -l 50:10 -t 6500:5000> /dev/null &
udiskie &
xmodmap .Xmodmap &
xscreensaver -no-splash &

nitrogen --restore &

exec i3
