STOWFLAGS =
TARGET = $(HOME)
STOW = stow $(STOWFLAGS) -R --target $(TARGET)

.PHONY: all

all: apps base devel mail ui

apps:
	$(STOW) ipython irssi mpd
base:
	$(STOW) emacs peco vim zsh
devel:
	$(STOW) code db haskell git
mail:
	$(STOW) afew
ui:
	$(STOW) dunst i3 tex x11
