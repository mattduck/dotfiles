# Use colour with ls by default - the way people do this seems
# to be just through an alias.
if ls --color > /dev/null 2>&1; then # GNU `ls`
    lscolorflag="--color"
else # OS X `ls`
    lscolorflag="-G"
fi

# General aliases
alias ls="ls ${lscolorflag}"
alias l="ls -hlF"
alias ll="ls -halF"
alias sudo="sudo " # Enable aliases to be sudo'ed
alias grep="grep --color=auto"
alias g="git"
alias gg="git status"
alias k="kubectl"
alias dockerc="docker-compose"
alias ..="cd .."
alias vi="nvim"
alias vim="nvim"
