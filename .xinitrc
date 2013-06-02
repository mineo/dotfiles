xmodmap .Xmodmap &
urxvtd -q -o -f
killall mpdscribble
mpdscribble
dropboxd &
redshift -l 50:10 -t 6500:5000 > /dev/null &
udiskie &
nitrogen --restore &
if [ $(hostname) = "slartibartfass" ]; then
    xrandr --output DVI-I-1 --mode 1440x900 --pos 0x0 --rotate normal --output DVI-I-0 --off --output HDMI-0 --mode 1600x1200 --pos 1440x0 --rotate left
    case $2 in
        xfce) exec startxfce4;;
        *)    exec i3;;
    esac
else
    exec i3
fi
