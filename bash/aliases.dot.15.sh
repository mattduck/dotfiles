# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    lscolorflag="--color"
else # OS X `ls`
    lscolorflag="-G"
fi

alias ls="ls ${lscolorflag}"
alias l="ls -hlF"
alias ll="ls -halF"

alias sudo="sudo " # Enable aliases to be sudo'ed

alias grep="grep --color=auto"

# Setup autocompletion for git alias
alias g="git"
alias gg="git status"
if [[ $(type -t __git_complete) == *function* ]]; then
    __git_complete g __git_main
fi

alias k="kubectl"
alias f="ranger"
alias r="rg"

alias ..="cd .."
alias ...="cd ../.."

alias dockerc="docker-compose"

alias ,bashrc=". ~/.bashrc"

export BAT_THEME="ansimatt"
if command -v bat >/dev/null; then
    alias cat="bat"
fi

alias yd="ydiff -s --wrap -w 120"
