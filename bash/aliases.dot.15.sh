# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    lscolorflag="--color"
else # OS X `ls`
    lscolorflag="-G"
fi

alias ls="ls ${lscolorflag}"
alias l="ls -lF"
alias ll="ls -alF"
alias lsd="ls -lF ${colorflag} | grep --color=never '^d'" # List only directories

alias sudo="sudo " # Enable aliases to be sudo'ed

alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
alias ag="ag --color-line-number=36 --color-path=32 --color-match=1\;4\;31"

# Setup autocompletion for git alias
alias g="git"
alias gg="git status"
if [[ $(type -t __git_complete) == *function* ]]; then
    __git_complete g __git_main
fi

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~~="cd ~"
alias -- -="cd -"
