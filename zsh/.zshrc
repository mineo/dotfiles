[ -e $HOME/.zshrc.local ] && . $HOME/.zshrc.local

function binary_exists () {
    type $1 &> /dev/null
}

function load_zle_widget () {
    autoload -Uz $1
    zle -N $1
}

autoload -Uz is-at-least

autoload -Uz colors
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

if is-at-least 5.2.0 $ZSH_VERSIN; then
    # ** search recursively, like **/*, but with less typing
    setopt GLOB_STAR_SHORT
fi

# History
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zhistory

setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS

# Prevent me from doing stupid things with `rm *`
unsetopt RM_STAR_SILENT
setopt RM_STAR_WAIT

[ -f ~/.dircolors ] && eval $(dircolors -b ~/.dircolors)
if binary_exists virtualenvwrapper_lazy.sh; then
    source $(command -v virtualenvwrapper_lazy.sh)
    alias wo='workon'
fi

# Easier URL typing
# url-quote-magic automatically escapes some characters as they are typed if
# they are part of a URL.
load_zle_widget url-quote-magic

if is-at-least 5.0.0 $ZSH_VERSION; then
    # Easier URL pasting
    # Like the above, but escapes characters as URLs are pasted.
    load_zle_widget bracketed-paste-magic
fi

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

if is-at-least 5.0.0 $ZSH_VERSION; then
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


if is-at-least 5.0.0 $ZSH_VERSION; then
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

if binary_exists jq; then
    alias -g J='|jq'
else
    alias -g J='|python -m json.tool'
fi

alias -g L='|less -R'

alias cps='rsync -ah --info=progress2'
alias cpui='cpupower frequency-info'
alias dmesg='dmesg -T'
alias ftphere='python2 -m pyftpdlib -w -d .'

if binary_exists hub; then
    alias g='hub'
else
    alias g='git'
fi

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

if binary_exists exa; then
    alias ls='exa --long --header'
else
    alias ls='ls -lh --color=auto'
fi

if binary_exists automp; then
    alias mp='automp'
else
    alias mp='mpv'
fi

if binary_exists brew; then
    [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
else
    [[ -s  /etc/profile.d/autojump.sh ]] && . /etc/profile.d/autojump.sh
fi

alias grep='grep --color=auto'
alias rg='rg --smart-case'

if binary_exists pacman; then
    alias fu='sudo pacman -Rns'
    alias pkg='pacman -Qi'
    alias sp='sudo pacman'
    alias pm='pacman'
    alias pa='pacaur'
fi

if binary_exists python3; then
    alias httphere='python3 -m http.server'
elif binary_exists python2; then
    alias httphere='python2 -m BaseHTTPServer'
fi


alias sc='schedtool -n 19 -B -e'

if binary_exists systemctl; then
    alias scs='systemctl status'
    alias ssc='sudo systemctl'
fi

alias rs='source ~/.zshrc'

# cdr: Remember recent directories, `cdr <TAB>` opens a list of them
if is-at-least 5.0.0 $ZSH_VERSION; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
fi
zstyle ':completion:*:*:cdr:*:*' menu selection

##############
# completion #
##############
# The following lines were added by compinstall

if is-at-least 5.0.6 $ZSH_VERSION; then
    zstyle ':completion:*' completer _expand _expand_alias _extensions _complete _ignored _approximate
else
    zstyle ':completion:*' completer _expand _expand_alias _complete _ignored _approximate
fi

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

load_zle_widget edit-command-line
bindkey -M emacs "^x^e" edit-command-line
bindkey -M viins "^x^e" edit-command-line
bindkey -M vicmd v edit-command-line

load_zle_widget history-beginning-search-menu
bindkey "^x^r" history-beginning-search-menu

load_zle_widget insert-files
bindkey "^x^f" insert-files

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete # Shift-Tab in completion

for i in /usr/share/zsh/plugins/*/*.zsh(N); do
    source $i
done

for i in /usr/local/share/zsh-*/*.zsh(N); do
    source $i
done

unset i

# tramp in emacs
[ ${TERM:-dumb} = "dumb" ] && unsetopt zle && PS1='$ ' && RPROMPT=''

# View the man page passed as the single argument in emacs.
function eman () {
    emacsclient -nc -a "" -eval "(man \"$1\")"
}

# View the JSON file passed as the single argument in less after colorizing it
# with jq. If jq is not available, fall back to simple formatting via Python's
# json.tool.
function jless () {
    if binary_exists jq; then
        jq -C . < $1 | less
    else
        python -m json.tool $1 | less
    fi
}

# Mkdir $1 and cd into it.
function mkcd () {
    mkdir -p $1 && cd $1
}

# Jump to the directory of the currently playing song.
function mpd-song-dir () {
    local song_dir="$(dirname "$(/usr/bin/mpc --no-status --format %file% current)")"
    cd ${HOME}/Musik/${song_dir}
}

# Git clone $1 in /tmp and cd into the cloned folder
function tmpclone {
    cd /tmp
    g clone $1
    cd ${${1##*/}%%.git}
}
