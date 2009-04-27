#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

# exec gnome-session
# exec startkde
# exec startxfce4
# exec icewm
# exec openbox
# exec blackbox
# exec fluxbox
# exec ratpoison
# exec dwm
# ... or any other WM of your choosing ...
exec /usr/bin/xmodmap /home/wieland/.Xmodmap&
#exec /usr/bin/bash&
#exec conky&
#exec gmpc --start-hidden&
#exec fbsetbg -l&
#exec trayer --widthtype request --height 10 --expand true --transparent true --align right --edge top&
#DEFAULT_SESSION=openbox-session
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
#exec wmii
