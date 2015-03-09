TARGET = $(HOME)
STOW = stow --target $(TARGET)

.PHONY: all

all: apps base devel ui

apps:
	$(STOW) ipython irssi mpd
base:
	$(STOW) emacs peco vim zsh
devel:
	$(STOW) code git db
ui:
	$(STOW) dunst i3 tex x11
