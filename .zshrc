# don't push something twice
setopt pushd_ignore_dups

autoload colors
colors

setopt autocd

setopt correct_all

export PS1="%{$fg[red]%n%} %{$fg[white]%} in %~ %{$fg[red]»%} %{$fg[white]%}  " 

#allow /etc/**/bla

for file in .zsh/*
do
	. $file
done
