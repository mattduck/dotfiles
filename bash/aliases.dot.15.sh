alias sudo="sudo " # Enable aliases to be sudo'ed

alias ls="ls --color=auto" # This breaks on mac if it doesn't use coreutils
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"

alias la="ls -A"
alias l="ls -CF"
alias ll="ls -alF"

alias h="history"

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
