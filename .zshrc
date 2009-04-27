# don't push something twice
setopt pushd_ignore_dups

# don't kill jobs when exiting shell 
setopt no_hup

# show us when some command didn't exit with 0
setopt print_exit_value

autoload colors
colors
# .. -> cd ../
setopt autocd

setopt correct_all

export PS1="%n@%~: " 

# cd /etc/**/foo/blub searches ;)
setopt extendedglob

export HISTFILE=~/.zsh_histfile

# for some odd reason allocating RAM to save the histfile takes ages..
export HISTSIZE=4000
export SAVEHIST=4000

for file in .zsh/**
do
	. $file
done
