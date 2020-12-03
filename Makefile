STOWFLAGS = --no-folding
TARGET = $(HOME)
BREW = brew
PIP  = pip3
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

OS := $(shell uname)

ifeq ($(OS), Darwin)
	OS_TARGET = all-osx
else
	OS_TARGET = all-linux
endif

FOLDERS = $(shell find . -maxdepth 1 -mindepth 1 -type d -not -name osx -print)

.PHONY: all all-linux all-osx osxsoftware osxupdate $(FOLDERS) clean

all: $(OS_TARGET)

all-linux: apps base devel linux ui

all-osx: apps base devel osxsoftware

apps: ipython irssi mpd mpv
archlinux: abs
base: spacemacs tmux vim zsh
devel: code db git
linux: systemd_user
ui: dunst gtk i3 sway tex rofi x11

osxsoftware:
	$(BREW) bundle --file=osx/Brewfile
	$(PIP) install -r osx/requirements.txt

osxupdate:
	$(BREW) update
	$(BREW) upgrade
	$(PIP) install --upgrade -r osx/requirements.txt

$(FOLDERS):
	$(STOW) $@

clean:
	$(STOW) -D $(FOLDERS)
