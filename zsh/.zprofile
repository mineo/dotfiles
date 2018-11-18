. /etc/zsh/zprofile

export BROWSER=firefox
export EDITOR=vim
export GOPATH=~/dev/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN
export PATH=~/.local/bin:~/.cabal/bin:/opt/java/jre/bin:/home/wieland/dev/bin:$PATH
export REPORTTIME=5
export RUST_SRC_PATH=~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
export WORKON_HOME="~/dev/virtualenvs/"
export XDG_DESKTOP_DIR="~/.desktop"
export XDG_DOWNLOAD_DIR="~/Downloads"

export BAT_THEME="Monokai Extended Light"
export LESS="-R"

if type source-highlight-esc.sh &> /dev/null; then
    export LESSOPEN="|source-highlight-esc.sh %s"
fi

[ -e $HOME/.zprofile.local ] && . $HOME/.zprofile.local
