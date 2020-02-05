export BROWSER=firefox
export EDITOR=vim
export GOPATH=~/dev/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN
export PATH=~/.local/bin:~/.cabal/bin:/opt/java/jre/bin:/home/wieland/dev/bin:$PATH

if [[ "${VENDOR}" == apple ]]; then
    for p in ~/Library/Python/*/bin(/N); do
        export PATH=$p:$PATH
    done
fi

export REPORTTIME=5
export RUST_SRC_PATH=~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
export WORKON_HOME="~/dev/virtualenvs/"
export XDG_DESKTOP_DIR="~/.desktop"
export XDG_DOWNLOAD_DIR="~/Downloads"

export BAT_STYLE="plain"
export BAT_THEME="Monokai Extended Light"
export LESS="-R"
export RIPGRE_CONFIG_PATH="~/.config/ripgrep"

export MOSH_TITLE_NOPREFIX=1

if type source-highlight-esc.sh &> /dev/null; then
    export LESSOPEN="|source-highlight-esc.sh %s"
fi

[ -e $HOME/.zprofile.local ] && . $HOME/.zprofile.local
