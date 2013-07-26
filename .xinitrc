xmodmap .Xmodmap &
urxvtd -q -o -f
killall mpdscribble
mpdscribble
dropboxd &
redshift -l 50:10 -t 6500:5000 > /dev/null &
udiskie &
synapse -s &
urxvtc -name scratchpad -e bash -c "ncmpcpp" &
gajim &
urxvtc -e bash -c "tmux -q attach-session -d -t irc || tmux new-session -s irc 'tmux source-file ~/.tmux-irc.conf'"
if [ $(hostname) = "slartibartfass" ]; then
    xrandr --output DVI-I-1 --mode 1440x900 --pos 0x0 --rotate normal --output DVI-I-0 --off --output HDMI-0 --mode 1600x1200 --pos 1440x0 --rotate left
fi

nitrogen --restore &

    case $2 in
        xfce) exec startxfce4;;
        *)    exec i3;;
    esac
