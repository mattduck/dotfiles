export EDITOR="vim"

export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="ls:l:ll:h:cd:vi:history:,history:gg:"
export HISTCONTROL="ignorespace:ignoredups"
shopt -s histappend

,path-add --prepend $HOME/bin

export VIRTUAL_ENV_DISABLE_PROMPT=1

export GREP_COLOR="1;4;31"
export GREP_OPTIONS="--color=auto"

if [ $(command -v keychain) ]; then
    eval `keychain --quiet --eval --agents "ssh" --inherit "local-once"`
fi
