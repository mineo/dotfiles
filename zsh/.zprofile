export BROWSER=firefox
export EDITOR=vim
export GOPATH="$HOME"dev/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN
export PATH="$HOME"/.local/bin:"$HOME"/.cabal/bin:"$HOME"/.cargo/bin/:/opt/java/jre/bin:"$HOME"/dev/bin:$PATH

if [[ "${VENDOR}" == apple ]]; then
    for p in ~/Library/Python/*/bin(/N); do
        export PATH=$p:$PATH
    done
fi

export REPORTTIME=5
export RUST_SRC_PATH="$HOME"/rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
export WORKON_HOME="$HOME/dev/virtualenvs/"
export XDG_DESKTOP_DIR="$HOME/.desktop"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"

export BAT_STYLE="plain"
export BAT_THEME="Monokai Extended Light"
export LESS="--RAW-CONTROL-CHARS --jump-target=.5 --ignore-case"


[ -e "$HOME"/.config/ripgrep] && export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep"

export MOSH_TITLE_NOPREFIX=1

if type source-highlight-esc.sh &> /dev/null; then
    export LESSOPEN="|source-highlight-esc.sh %s"
fi

[ -e "$HOME"/.zprofile.local ] && . "$HOME"/.zprofile.local
