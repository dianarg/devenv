# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias emacs="emacs -nw"
export PATH=$PATH:$HOME/scripts

HISTSIZE=100000
HISTTIMEFORMAT="%y/%m/%d %T "

# git autocomplete
source ~/scripts/git-completion.bash

# custom configuration for this system
shopt -s nullglob
for f in $HOME/scripts/bash_config/*; do
	. "$f"
done
