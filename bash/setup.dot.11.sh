# Many programs consult this when they need to open a text editor
export EDITOR="vim"

# Allow $HISTFILE to grow to 100k lines, and allow the `history` command to
# access all of them rather than truncating
export HISTFILESIZE=100000

# When an interactive shell exits, the last $HISTSIZE lines in the command
# history are written to $HISTFILE
export HISTSIZE=$HISTFILESIZE

# Commands to ignore in the history file. ? and ?? ignore trival one-letter and
# two-letter commands.
export HISTIGNORE="?:??:ls:l:ll:h:cd:vi:vim:history:,history:g:gg:g d:tmux:tmux attach"

# - ignorespace: lines which begin with a space character are not saved in the
#   history list.
# - ignoredups: lines matching the previous line are not saved
export HISTCONTROL="ignorespace:ignoredups"

# Append to the $HISTFILE when an interactive shell exits, instead of
# overwriting it.
shopt -s histappend

# Shell should attempt to save each line of a multi-line command in the same
# history entry, adding semicolons where necessary to preserve syntactic
# correctness.
shopt -s cmdhist

# ~/bin takes precedence over most of the $PATH
,path-add --prepend $HOME/bin

# Create neither *.pyc files nor __pycache__ directories.
export PYTHONDONTWRITEBYTECODE=1

# By default, Python's virtualenv will modify the prompt when a virtualenv is
# active. Disable this.
export VIRTUAL_ENV_DISABLE_PROMPT=1

# TODO - what was the reason I had to use this in the past?
if [ $(command -v keychain) ]; then
    eval `keychain --quiet --eval --agents "ssh" --inherit "local-once"`
fi
