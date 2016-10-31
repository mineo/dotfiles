if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

dropbox &
redshift -l 50:10 -t 6500:5000> /dev/null &
udiskie &
setxkbmap -layout us -model pc105 -option ctrl:swapcaps,compose:lctrl
xmodmap .Xmodmap
xscreensaver -no-splash &
dunst &
parcellite &
nitrogen --restore &

exec i3
