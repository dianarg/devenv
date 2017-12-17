# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias emacs="emacs -nw"
alias start-emacs-daemon="emacs --daemon"
alias ce="emacsclient -t"
alias kill-emacs-daemon="emacsclient -e \"(kill-emacs)\""
alias restart-emacs-daemon="kill-emacs-daemon; emacs --daemon"
alias ls="ls --color=auto"

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

SAVE=$PS1

# from Chris Cantalupo
__git_ps1 () {
    local b
    b=`git symbolic-ref --short HEAD 2> /dev/null`
    if  [ -n "$b" ]; then
	printf "(%s)" "${b##refs/heads/}";
    fi
}

# custom prompt
PS1="\[\e[44;1;31m\]\t \[\e[45;1;33m\] \u@\h \[\e[41;1;32m\] \W \$(__git_ps1) \[\e[44;1;35m\] >\[\e[40;1;37m\] "
