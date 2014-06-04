export EDITOR="vim"

export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="ls:l:ll:h:cd:"
export HISTCONTROL="ignorespace:ignoredups"
shopt -s histappend

,path-add $HOME/bin

export VIRTUAL_ENV_DISABLE_PROMPT=1

if [ $(command -v keychain) ]; then
    keychain --quiet
    eval `keychain --quiet --eval`
fi
