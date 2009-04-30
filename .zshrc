# don't push something twice
setopt pushd_ignore_dups


# show us when some command didn't exit with 0
setopt print_exit_value

#autoload colors
#colors
# .. -> cd ../
setopt autocd

setopt correct_all

export PS1="%n@%~: " 

#allow /etc/**/bla
#setopt extendedglob

export HISTFILE=~/.zsh_histfile

for file in .zsh/*
do
	. $file
done
