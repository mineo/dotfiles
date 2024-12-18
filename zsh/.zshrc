function binary_exists () {
    type $1 &> /dev/null
}

[ -e $HOME/.zshrc.local ] && . $HOME/.zshrc.local

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
setopt MENU_COMPLETE

if is-at-least 5.2.0; then
    # ** search recursively, like **/*, but with less typing
    setopt GLOB_STAR_SHORT
fi

setopt EXTENDED_GLOB

# History
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zhistory

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY

# Complete filenames after equals signs.
setopt MAGIC_EQUAL_SUBST

# Prevent me from doing stupid things with `rm *`
unsetopt RM_STAR_SILENT
setopt RM_STAR_WAIT

# Prevent me from truncating existing files, but allow it through commands form
# the history
setopt NOCLOBBER
setopt HIST_ALLOW_CLOBBER

if is-at-least 5.1.0; then
    setopt APPEND_CREATE
fi

[ -f ~/.dircolors ] && eval $(dircolors -b ~/.dircolors)
if binary_exists virtualenvwrapper_lazy.sh; then
    source $(command -v virtualenvwrapper_lazy.sh)
    alias wo='workon'
fi

# Easier URL typing
# url-quote-magic automatically escapes some characters as they are typed if
# they are part of a URL.
load_zle_widget url-quote-magic

if is-at-least 5.0.0; then
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

if is-at-least 5.0.0; then
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
    esac

    if [[ -n "${STY}" ]]; then
        printf '\ek%s\e\\' $1
    fi
}

if is-at-least 5.0.0 && [[ ${TERM:-dumb} != "dumb" && ! ${TERM} =~ "eterm*" ]]; then
    PROMPT='%(V,%1v ,)%1{↪%} '
    RPROMPT='%F{yellow}%~${vcs_info_msg_0_}%f «%(0?.. [%?] «) %(!.%U%K{red}%F{black}.%F{yellow})%n%u%k %fon %F{magenta}%m%f'
elif [[ $TERM == "dumb" ]]; then
    unsetopt zle
    PS1='$ '
else
    PROMPT=$'%{\e[0;33m%}%n %{\e[0;10m%}on %{\e[0;35m%}%m%{\e[0;10m%}: %~%(0?.. [%?]) %1{↪%} '
fi
ZLE_RPROMPT_INDENT=0

###########
# aliases #
###########
if binary_exists bat; then
    alias b='bat'
fi

alias fda='fd --no-ignore --hidden'

if binary_exists fzf; then
    alias -g F='|fzf --multi'
fi

if binary_exists rg; then
    if [[ $(systemd-detect-virt) == wsl ]]; then
        alias -g G='|\rg --smart-case --hyperlink-format=vscode://file//wsl.localhost/Ubuntu/{path}:{line}:{column}'
        alias -g GC='|\rg --smart-case --pretty --hyperlink-format=vscode://file//wsl.localhost/Ubuntu/{path}:{line}:{column}'
        alias rgc='rg --pretty --hyperlink-format=vscode://file//wsl.localhost/Ubuntu/{path}:{line}:{column}'
        alias rg='rg --smart-case --hyperlink-format=vscode://file//wsl.localhost/Ubuntu/{path}:{line}:{column}'
    else
        alias -g G='|\rg --smart-case'
        alias -g GC='|\rg --smart-case --pretty'
        alias rgc='rg --pretty'
        alias rg='rg --smart-case'
    fi
else
    alias -g G='|grep'
fi

if binary_exists jq; then
    alias -g J='|jq'
    alias -g JL='|jq --color-output | less -R'
    alias jqc='jq --color-output'
else
    alias -g J='|python -m json.tool'
fi

alias -g L='|less -R'

alias cps='rsync -ah --info=progress2'
alias cpui='cpupower frequency-info'
alias dmesg='dmesg -T'

if binary_exists hub; then
    alias g='hub'
else
    alias g='git'
fi

alias hme='htop -u $(whoami)'
alias ip='ip -color -human'

if [[ "${VENDOR}" == apple ]]; then
    alias m='open'
else
    alias m='mimeopen'
fi
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

alias ll=ls

if [[ "${VENDOR}" == apple ]]; then
    [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
else
    [[ -s  /etc/profile.d/autojump.sh ]] && . /etc/profile.d/autojump.sh
fi

alias grep='grep --color=auto'

if binary_exists pacman; then
    alias fu='sudo pacman -Rns'
    alias pkg='pacman -Qi'
    alias sp='sudo pacman'
    alias pm='pacman'
    alias pa='pacaur'
fi

if ! binary_exists tailf; then
    alias tailf='tail -f'
fi

httphere() {
    printf "The current hostname is %s\n" $(hostname --fqdn)
    if binary_exists python3; then
        python3 -m http.server $@
    elif binary_exists python2; then
        python2 -m BaseHTTPServer $@
    fi
}

ftphere() {
    printf "The current hostname is %s\n" $(hostname --fqdn)
    python3 -m pyftpdlib -w -d . $@
}


alias sc='schedtool -n 19 -B -e'

if binary_exists systemctl; then
    alias scs='systemctl status'
    alias ssc='sudo systemctl'
fi

alias rs='source ~/.zshrc'

# cdr: Remember recent directories, `cdr <TAB>` opens a list of them
if is-at-least 5.0.0; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
fi
zstyle ':completion:*:*:cdr:*:*' menu selection

##############
# completion #
##############
# The following lines were added by compinstall

if is-at-least 5.0.6; then
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
# and insert the chosen section in case a man page is available in multiple
zstyle ':completion:*:*' insert-sections true
# from https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/completion.zsh
zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,cmd -w -w"
zstyle :compinstall filename '/home/wieland/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

bindkey -e

# Like history-beginning-search-{back,for}ward, but place the cursor at the end
# of the line.
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "^[[A" history-beginning-search-backward-end # up
bindkey "^[[B" history-beginning-search-forward-end # down
# on C-p / C-n as well
bindkey "^P" history-beginning-search-backward-end # up
bindkey "^N" history-beginning-search-forward-end # down
# tmux on my raspberry pi
bindkey "^[OA" history-beginning-search-backward-end # up
bindkey "^[OB" history-beginning-search-forward-end # down
bindkey "[D" backward-word # left
bindkey "[C" forward-word # right

bindkey "^[[7~" beginning-of-line # Home
bindkey "^[[8~" end-of-line # End
bindkey "^[[1~" beginning-of-line # Home
bindkey "^[[4~" end-of-line # End
bindkey "^[[5~" beginning-of-history # PageUp
bindkey "^[[6~" end-of-history # PageDown
bindkey "^[[3~" delete-char # Del
bindkey "^[/"  _history-complete-older # M-/

# Alt+{Left,Right} in GNOME terminal
bindkey '^[[1;3D' backward-word
bindkey '^[[1;3C' forward-word

# The default backward-kill-word is not as useful when operating on paths: On
# "ls /usr/bin/gcc|" (| being the point), backward-kill-word will kill the
# whole path. vi-backward-kill-word only kills "gcc", which is much nicer.
bindkey "^W" vi-backward-kill-word

# Possibly rebound for fzf later on
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


# Arch Linux
for i in /usr/share/zsh/plugins/*/*.zsh(N); do
    source $i
done

# raspbian
for i in /usr/share/zsh-*/*.zsh(N); do
    source $i
done

# macOS
for i in /usr/local/share/zsh-*/*.zsh(N); do
    source $i
done

# On some machines I manually manage extra files for zsh.
if [[ -n "${ZSHEXTRADIR}" ]]; then
    for i in "${ZSHEXTRADIR}"/*.zsh(N); do
        source $i
    done
fi


unset i

if binary_exists fzf; then
    # Arch Linux
    if [[  -d /usr/share/fzf/ ]]; then
        source /usr/share/fzf/completion.zsh
        source /usr/share/fzf/key-bindings.zsh
    fi

    # macOS
    if [[ -e /usr/local/opt/fzf/shell/ ]]; then
        source /usr/local/opt/fzf/shell/completion.zsh
        source /usr/local/opt/fzf/shell/key-bindings.zsh
    fi
fi

# View the man page passed as the single argument in emacs.
function eman () {
    emacsclient -nc -a "" -eval "(man \"$1\")"
}

# View the JSON file passed as the single argument in less after colorizing it
# with jq. If jq is not available, fall back to bat (if available) or simple
# formatting via Python's json.tool.
function jless () {
    if binary_exists jq; then
        jq -C . < $1 | less
    elif binary_exists bat; then
        bat $1
    else
        python -m json.tool $1 | less
    fi
}

# Mkdir $1 and cd into it.
function mkcd () {
    mkdir -p $1 && cd $1
}

if binary_exists mpc; then
    # Jump to the directory of the currently playing song.
    function mpd-song-dir () {
        local song_dir="$(dirname "$(/usr/bin/mpc --no-status --format %file% current)")"
        cd ${HOME}/Musik/${song_dir}
    }
fi

if binary_exists exiv2; then
    # Rename all passed filenames with `exiv2`.
    function exivrename {
        exiv2 --Force --rename '%Y-%m-%d %H:%M:%S' mv "${@:?}"
    }
fi


if binary_exists sponge; then
    if binary_exists jq; then
        # Format a JSON file in-place.
        function jsonfmt {
            jq . < ${1:?} | ifne sponge ${1:?}
        }
    fi

    if binary_exists xmllint; then
        # Format an XML file in-place.
        function xmlfmt {
            xmllint --format ${1:?} | sponge ${1:?}
        }
    fi
fi

# Git clone $1 in /tmp and cd into the cloned folder
function tmpclone {
    cd /tmp
    g clone $1
    cd ${${1##*/}%%.git}
}
