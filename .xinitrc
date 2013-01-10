xmodmap .Xmodmap &
urxvtd -q -o -f
killall mpdscribble
mpdscribble
dropboxd &
fehlstart &
redshift -l 50:10 -t 6500:5000 > /dev/null
udiskie &
if [ $(hostname) = "slartibartfass" ]; then
    xrandr --output DVI-I-1 --mode 1440x900 --pos 0x0 --rotate normal --output DVI-I-0 --off --output HDMI-0 --mode 1600x1200 --pos 1440x0 --rotate left
    exec i3
else
    exec i3
fi
