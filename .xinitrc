#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

exec /usr/bin/xmodmap /home/wieland/.Xmodmap&
DEFAULT_SESSION=wmii
case $1 in
fluxbox)
	exec fluxbox
	;;
wmii)
	exec wmii
	;;
openbox)
	exec openbox-session
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
