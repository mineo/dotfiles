if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

if [ $(hostname) = "slartibartfass" ]; then
    xrandr --output DVI-I-1 --mode 1440x900 --pos 0x0 --rotate normal --output DVI-I-0 --off --output HDMI-0 --mode 1600x1200 --pos 1440x0 --rotate left
fi

dropboxd &
gajim &
redshift -l 50:10 -t 6500:5000> /dev/null &
udiskie &
xmodmap .Xmodmap &
xscreensaver -no-splash &
dunst &

nitrogen --restore &

exec i3
