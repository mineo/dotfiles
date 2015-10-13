STOWFLAGS =
TARGET = $(HOME)
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

.PHONY: all

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
ui:
	$(STOW) dunst gtk i3 tex x11
