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

# make terminal emacs colors look better with xterm-256-color
# on WSL, only xterm-color works
if grep -q Microsoft /proc/version; then
    export TERM=xterm-color
else
    export TERM=xterm-256color
fi

# git autocomplete
source ~/scripts/git-completion.bash

# custom configuration for this system
for f in $HOME/scripts/bash_config/*sh; do
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
PS1="\[\e[1;31m\]\t \[\e[1;33m\] \u@\h \[\e[1;32m\] \W \$(__git_ps1) \[\e[1;35m\] >\[\e[1;37m\] "

# simple prompts for screen shots
#PS1="[\[\e[36m\]\u\[\e[m\]@\[\e[37m\]\h:\[\e[33m\]\W\[\e[m\]] $ "
#PS1="[\[\e[36m\]user\[\e[m\]@\[\e[37m\]\h:\[\e[33m\]\W\[\e[m\]] $ "
