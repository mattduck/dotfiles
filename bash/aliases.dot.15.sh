# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    lscolorflag="--color"
else # OS X `ls`
    lscolorflag="-G"
fi

alias ls="ls ${lscolorflag}"
alias l="ls -hlF"
alias ll="ls -halF"
alias lsd="ls -hlF ${colorflag} | grep --color=never '^d'" # List only directories

alias sudo="sudo " # Enable aliases to be sudo'ed

alias ag="ag --color-line-number=36 --color-path=32 --color-match=1\;4\;31"
alias grep="grep --color=auto"

# Setup autocompletion for git alias
alias g="git"
alias gg="git status"
if [[ $(type -t __git_complete) == *function* ]]; then
    __git_complete g __git_main
fi
complete -o bashdefault -o default -o nospace -F _fzf_path_completion g

alias k="kubectl"

alias ..="cd .."
alias ...="cd ../.."

# Resolve symlinks
alias cdp="cd -P"

alias v="vim"
alias f=",cd"

alias vssh="vagrant ssh || vagrant up && vagrant ssh"

alias dockerc="docker-compose"

alias ,bashrc=". ~/.bashrc"
