#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

exec /usr/bin/xmodmap /home/wieland/.Xmodmap&
exec urxvtd -q -o -f &
#exec xflux -l 51 &
#exec zim &
#exec nitrogen --restore &
DEFAULT_SESSION=wmii
case $1 in
fluxbox)
	exec fluxbox
	;;
wmii)
	exec wmii
	;;
openbox)
	exec openbox-sessizon
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
