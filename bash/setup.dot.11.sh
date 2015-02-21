export EDITOR="vim"

export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="ls:l:ll:h:cd:"
export HISTCONTROL="ignorespace:ignoredups"
shopt -s histappend

,path-add --prepend $HOME/bin

export VIRTUAL_ENV_DISABLE_PROMPT=1

if [ $(command -v keychain) ]; then
    eval `keychain --quiet --eval --agents "ssh" --inherit "local-once"`
fi
