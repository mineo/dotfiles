STOWFLAGS =
TARGET = $(HOME)
BREW = brew
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

.PHONY: all osx

all: apps base devel mail ui

apps:
	$(STOW) ipython irssi mpd
archlinux:
	$(STOW) abs
base:
	$(STOW) spacemacs peco vim zsh
devel:
	$(STOW) code db haskell git
mail:
	$(STOW) afew mutt
osx:
	$(BREW) bundle --file=osx/Brewfile
ui:
	$(STOW) dunst gtk i3 tex rofi x11
