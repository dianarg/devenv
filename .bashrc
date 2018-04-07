# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias emacs="emacs -nw"
alias emacs-daemon="emacs --daemon"
alias emacsc="emacsclient -t"
alias kill-emacs-daemon="emacsclient -e \"(kill-emacs)\""
alias restart-emacs-daemon="emacsclient -e \"(kill-emacs)\"; emacs --daemon"

PATH=$PATH:$HOME/scripts

HISTSIZE=100000
HISTTIMEFORMAT="%y/%m/%d %T "

# make terminal emacs colors look better
TERM=xterm-256color

# git autocomplete
source ~/scripts/git-completion.bash

# custom configuration for this system
for f in $HOME/scripts/bash_config/*; do
	. "$f"
done
