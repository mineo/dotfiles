STOWFLAGS =
TARGET = $(HOME)
BREW = brew
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

OS := $(shell uname)

ifeq ($(OS), Darwin)
	OS_TARGET = all-osx
else
	OS_TARGET = all-linux
endif

FOLDERS = $(shell find . -maxdepth 1 -mindepth 1 -type d -print)

.PHONY: all all-linux all-osx homebrew osxupdate $(FOLDERS) clean

all: $(OS_TARGET)

all-linux: apps base devel linux ui

all-osx: apps base devel homebrew

apps: ipython irssi mpd mpv
archlinux: abs
base: spacemacs tmux vim zsh
devel: code db git
linux: systemd_user
ui: dunst gtk i3 tex rofi x11

homebrew:
	$(BREW) bundle --file=osx/Brewfile
osxupdate:
	$(BREW) update
	$(BREW) upgrade

$(FOLDERS):
	$(STOW) $@

clean:
	$(STOW) -D $(FOLDERS)
