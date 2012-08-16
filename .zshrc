autoload colors
colors
setopt autocd
setopt autopushd
setopt pushd_silent
setopt cdablevars
setopt correct_all
setopt prompt_subst

# VCS_INFO stuff
# set formats
# # %b - branchname
# # %u - unstagedstr (see below)
# # %c - stangedstr (see below)
# # %a - action (e.g. rebase-i)
# # %R - repository path
# # %S - path in the repository
# # %s - vcs in use
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "[u]"
zstyle ':vcs_info:*' stagedstr "[s]"
zstyle ':vcs_info:*' actionformats " (%b)%u-%a"
zstyle ':vcs_info:*' formats       " (%b)%u"
autoload -Uz vcs_info
vcs_info
precmd () {
    vcs_info
    [[ -n $vcs_info_msg_0_ ]] && psvar[1]="$vcs_info_msg_0_"
    print -Pn "e]0;%n: %~"
}
preexec() {
    # define screen/terminal title with the current command (http://aperiodic.net/phil/prompt/)
    case $TERM in
      rxvt-*)
          printf '\33]2;%s\007' $1
      ;;
      screen*)
        printf '\ek%s\e\\' $1;;
    esac
}

case $TERM in
    rxvt-*)
        chpwd(){
            print -Pn "\e]2;%n: %~\a"
        }
        ;;
    screen*)
        chpwd(){
            print -Pn "\e%~\a"
        }
        ;;
esac


PROMPT="%{$fg[yellow]%}%n %{$fg[white]%}on %{$fg[yellow]%}%m %{$fg[white]%}in %{$fg[yellow]%}%~%{$fg[red]%}%v%{$fg[white]%}
»%{$fg[white]%}"
build_aur() {
        wget http://aur.archlinux.org/packages/$1/$1.tar.gz || exit 1
        tar xf $1.tar.gz || exit 1
        cd $1 || exit 1 
        makepkg -rcsi || exit 1
        cd ..
        rm -rf $1 && rm $1.tar.gz || exit 1
}

###########
# aliases #
###########
alias -g L='|less -R'
alias -g G='|grep'
alias home='cd ~'
alias rmdir='rm -R'
alias scr='screen -RDx'
alias v='vim'
alias sv='sudo vim'
alias q='exit'

alias q150='ssh wieland@q150'
alias mpfb='mplayer -vo fbdev -zoom -xy 1024:768 -fs'
alias less='less -N'
alias mp='mplayer'
alias mp51='mplayer -ao alsa -channels 6 -af pan=2:1:0:0:1:1:0:0:1:0.707:0.707:1:1'
# channels:left-fl:right-fl:left-fr:right-fr:left-sl:right-sl:left-sr:right-sr:left-fc:right-fc:left-lfe:right-lfe
alias cpui='cpupower frequency-info'
alias sucp='sudo cp'
alias qemu='qemu -enable-kvm'
alias wine32='WINEARCH=win32 WINEPREFIX=~/.wine32'
alias ipcons='ipython2 qtconsole --pylab=auto'
alias pypy='pypy -E'

alias ls='ls -lh --color=auto'
alias grep='grep --color=auto'
alias lsg='ls | grep'
alias shutdown='sudo shutdown -h now'

alias fu='sudo pacman -Rns'
alias pkg='pacman -Qi'
alias sp='sudo pacman'
alias pm='pacman'
alias p='packer'
alias sc='schedtool -n 19 -B -e'

alias blueman='blueman-manager'
alias m='udisks --mount'
alias um='udisks --unmount'

alias rs='source ~/.zshrc'

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
bindkey "^[[A" history-beginning-search-backward # up
bindkey "^[[B" history-beginning-search-forward # down
bindkey "^[[7~" beginning-of-line # Home
bindkey "^[[8~" end-of-line # End
bindkey "^[[5~" beginning-of-history # PageUp
bindkey "^[[6~" end-of-history # PageDown
bindkey "^[[3~" delete-char # Del
