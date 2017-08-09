STOWFLAGS =
TARGET = $(HOME)
BREW = brew
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

.PHONY: all-linux all-osx osx osxupdate

all-linux: apps base devel mail ui

all-osx: apps base devel osx

apps:
	$(STOW) ipython irssi mpd mpv
archlinux:
	$(STOW) abs
base:
	$(STOW) spacemacs tmux vim zsh
devel:
	$(STOW) code db git
mail:
	$(STOW) afew neomutt
osx:
	$(BREW) bundle --file=osx/Brewfile
osxupdate:
	$(BREW) update
	$(BREW) upgrade
	$(BREW) linkapps
ui:
	$(STOW) dunst gtk i3 tex rofi x11
