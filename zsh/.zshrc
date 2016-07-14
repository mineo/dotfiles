[ -e $HOME/.zshrc.local ] && . $HOME/.zshrc.local

autoload colors
colors
setopt autocd
setopt autopushd
setopt pushd_silent
setopt cdablevars
setopt correct_all
setopt prompt_subst
# Don't show a list of completions before letting me select one of them
unsetopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE

# If a glob pattern doesn't match anything, just leave it as is
# Useful when pasting URLs with question marks in them
unsetopt NOMATCH

# History
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zhistory

setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS

[ -f ~/.dircolors ] && eval $(dircolors -b ~/.dircolors)
command virtualenvwrapper_lazy.sh &> /dev/null && source $(command -v virtualenvwrapper_lazy.sh)

# Easier URL typing
# url-quote-magic automatically escapes some characters as they are typed if
# they are part of a URL.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

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
zstyle ':vcs_info:*' unstagedstr "%F{red}＊"
zstyle ':vcs_info:*' stagedstr "%F{green}＋"
zstyle ':vcs_info:*' actionformats " %F{green}(%b)%c%u-%a"
zstyle ':vcs_info:*' formats       " %F{green}(%b)%c%u"

if [[ $ZSH_VERSION > 5.0.0 ]]; then
    autoload -Uz vcs_info
    vcs_info
    precmd () {
        vcs_info
    }
fi

preexec() {
    # define screen/terminal title with the current command (http://aperiodic.net/phil/prompt/)
    case $TERM in
        rxvt-*)
            printf '\33]2;%s\007' $1
            ;;
        screen*)
            printf '\ek%s\e\\' $1
            ;;
    esac
}


if [[ $ZSH_VERSION > 5.0.0 ]]; then
    PROMPT='↪ '
    RPROMPT='%F{yellow}%~${vcs_info_msg_0_}%f «%(0?.. [%?] «) %F{yellow}%n %fon %F{magenta}%m%f'
else
    PROMPT=$'%{\e[0;33m%}%n %{\e[0;10m%}on %{\e[0;35m%}%m%{\e[0;10m%}: %~%(0?.. [%?]) ↪ '
fi
ZLE_RPROMPT_INDENT=0

###########
# aliases #
###########
alias -g G='|grep'
alias -g J='|jq'
alias -g L='|less -R'
alias -g P='|peco'

alias cps='rsync -ah --info=progress2'
alias cpui='cpupower frequency-info'
alias dmesg='dmesg -T'
alias ftphere='python2 -m pyftpdlib -w -d .'
alias g='git'
command -v hub &> /dev/null && alias g='hub'
alias hme='htop -u $(whoami)'
alias ipcons='ipython2 qtconsole --pylab=auto'
alias m='mimeopen'
alias mp='automp'
alias mutt='GPG_AGENT_INFO="" mutt'
alias pypy='pypy -E'
alias qemu='qemu -enable-kvm'
alias rmdir='rm -R'
alias sucp='sudo cp'
alias sv='sudoedit'
alias um='udisks --unmount'
alias v='vim'
alias wo='workon'

if type ls++ &> /dev/null; then
    alias ls='ls++'
else
    alias ls='ls -lh --color=auto'
fi

if type automp &> /dev/null; then
    alias mp='automp'
else
    alias mp='mpv'
fi

[[ -s  /etc/profile.d/autojump.sh ]] && . /etc/profile.d/autojump.sh

alias grep='grep --color=auto'

if type pacman &> /dev/null; then
    alias fu='sudo pacman -Rns'
    alias pkg='pacman -Qi'
    alias sp='sudo pacman'
    alias pm='pacman'
    alias pa='pacaur'
fi

alias sc='schedtool -n 19 -B -e'

if type systemctl &> /dev/null; then
    alias scs='systemctl status'
    alias ssc='sudo systemctl'
fi

alias rs='source ~/.zshrc'

# cdr: Remember recent directories, `cdr <TAB>` opens a list of them
if [[ $ZSH_VERSION > 5.0.0 ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
fi
zstyle ':completion:*:*:cdr:*:*' menu selection

##############
# completion #
##############
# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' squeeze-slashes true
# Colors like ls
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# What is completed (file/command/...)
zstyle ':completion:*' format 'Completing %d'
# For completion lists that have to be scrolled
zstyle ':completion:*' list-prompt 'Showing %l lines (%m matches)'
zstyle ':completion:*' select-prompt '%m matches'
# separate man pages into sections
zstyle ':completion:*:manuals' separate-sections true
# from https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/completion.zsh
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"
zstyle :compinstall filename '/home/wieland/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
bindkey "^[[A" history-beginning-search-backward # up
bindkey "^[[B" history-beginning-search-forward # down
bindkey "^[[7~" beginning-of-line # Home
bindkey "^[[8~" end-of-line # End
bindkey "^[[1~" beginning-of-line # Home
bindkey "^[[4~" end-of-line # End
bindkey "^[[5~" beginning-of-history # PageUp
bindkey "^[[6~" end-of-history # PageDown
bindkey "^[[3~" delete-char # Del
bindkey "^R" history-incremental-search-backward

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete # Shift-Tab in completion

if [ -d /usr/share/zsh/plugins ]; then
    for i in /usr/share/zsh/plugins/*/*.zsh; do
        source $i
    done
fi

# tramp in emacs
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && RPROMPT=''

function eman () {
    emacsclient -nc -a "" -eval "(man \"$1\")"
}

function mpd-song-dir () {
    local song_dir="$(dirname "$(/usr/bin/mpc --no-status --format %file% current)")"
    cd ${HOME}/Musik/${song_dir}
}
