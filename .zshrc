autoload colors
colors
setopt autocd
setopt correct_all
export PS1="%{$fg[red]%n%}%{$fg[white]%} in %~ %{$fg[red]»%}%{$fg[white]%} " 

###########
# aliases #
###########
alias fz='filezilla'
alias home='cd ~'
alias rmdir='rm -R'
alias scr='screen -rx'
alias v='vim'
alias sv='sudo vim'
alias yy='ftp_proxy=proxy:3128 yaourt'
alias y='yaourt'
alias q='exit'
alias mpfb='mplayer -vo fbdev -zoom -xy 1024:768 -fs'
alias mp='mplayer -ass -fs'
alias mp51='mplayer -ass -ao alsa -channels 3 -af hrtf'
alias cpui='cpufreq-info'
alias htop='htop -u `whoami`'
alias sucp='sudo cp'
alias tru='sudo truecrypt'


alias ll='ls -lh --color=tty'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias lsg='ls |fgrep'
alias psg='ps -e | grep'
alias shutdown='sudo shutdown -h now'
alias smblog='tail -n 30 /var/log/samba/log.smbd'

alias fu='yaourt -Rns'
alias upd='yaourt -Syu --aur '
alias pkg='yaourt -Qi'
alias gib='yaourt -S'
alias sync='yaourt -Sy'

alias up='cd ..'
alias rf='rm -rf'

alias rs='source ~/.zshrc'
alias wlzh='sudo netcfg zuhause'

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
