export EDITOR="vim"

export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="ls:l:ll:h:cd:"
export HISTCONTROL="ignorespace:ignoredups"
shopt -s histappend

,path-add $HOME/bin

export VIRTUAL_ENV_DISABLE_PROMPT=1
