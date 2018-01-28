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

FOLDERS = $(shell find . -maxdepth 1 -type d -print)

.PHONY: all all-linux all-osx homebrew osxupdate $(FOLDERS)

all: $(OS_TARGET)

all-linux: apps base devel linux ui

all-osx: apps base devel osx

apps:
	$(STOW) ipython irssi mpd mpv
archlinux:
	$(STOW) abs
base:
	$(STOW) spacemacs tmux vim zsh
devel:
	$(STOW) code db git
linux:
	$(STOW) systemd_user
homebrew:
	$(BREW) bundle --file=osx/Brewfile
osxupdate:
	$(BREW) update
	$(BREW) upgrade
	$(BREW) linkapps
ui:
	$(STOW) dunst gtk i3 tex rofi x11

$(FOLDERS):
	$(STOW) $@
