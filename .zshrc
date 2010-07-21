autoload colors
colors
setopt autocd
setopt correct_all

git_branch=`git branch 2>/dev/null | grep -e '^*' | sed -E 's/^\* (.+)$/(\1) /'`
PROMPT="%{$fg[yellow]%}%n %{$fg[white]%}in %{$fg[yellow]%}%~ %{$fg[white]%}» "
RPROMPT="«"

build_aur() {
        wget http://aur.archlinux.org/packages/$1/$1.tar.gz || exit 1
        tar xf $1.tar.gz || exit 1
        cd $1 || exit 1 
        makepkg -csi || exit 1
        cd ..
        rm -rf $1 && rm $1.tar.gz || exit 1
}

###########
# aliases #
###########
alias -g L='|less'
alias home='cd ~'
alias rmdir='rm -R'
alias scr='screen -rx'
alias v='vim'
alias sv='sudo vim'
alias q='exit'

alias q150='ssh wieland@q150'
alias mpfb='mplayer -vo fbdev -zoom -xy 1024:768 -fs'
alias mp='mplayer'
alias mp51='mplayer -ao alsa -channels 6 -af pan=2:1:0:0:1:1:0:0:1:0.707:0.707:1:1'
# channels:left-fl:right-fl:left-fr:right-fr:left-sl:right-sl:left-sr:right-sr:left-fc:right-fc:left-lfe:right-lfe
alias cpui='cpufreq-info'
alias htop='htop -u `whoami`'
alias sucp='sudo cp'
alias avdump='wine ~/Downloads/avdump.exe -1230e'

alias ls='ls -lh --color=auto'
alias grep='grep --color=auto'
alias lsg='ls | grep'
alias psg='ps -e | grep'
alias shutdown='sudo shutdown -h now'

alias fu='sudo pacman -Rns'
alias pkg='pacman -Qi'
alias sp='sudo pacman'
alias pm='pacman'
alias p='packer'

alias up='cd ..'
alias rf='rm -rf'

alias rs='source ~/.zshrc'

if [[ -x `which fortune` ]]; then
    fortune -a 2> /dev/null | cowsay -f three-eyes
fi

##############
# completion #
##############
# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt 'Completing %e'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle :compinstall filename '/home/wieland/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
