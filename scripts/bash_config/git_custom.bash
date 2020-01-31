# magic from Chris Cantalupo to display current git branch
__git_ps1 ()
{
    local b

    b="$(git symbolic-ref --short HEAD 2>/dev/null)";
    if [ -n "$b" ]; then
	printf " (%s)" "${b##refs/heads/}";
    fi
}
PS1='\u\[\e[1;30m\]@\[\e[1;35m\]\h\[\e[1;30m\]:\[\e[4;37m\]\w\[\e[0;33m\]$(__git_ps1)\[\e[0m\]]\[\e[1;37m\]\$\[\e[0m\] '

# another command from Chris to show recent commits and which branch they are on
alias git-blog='git for-each-ref --sort=-committerdate refs/heads | less'
