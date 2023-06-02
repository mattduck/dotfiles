# Set EDITOR
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="nvim"

# Allow $HISTFILE to grow to 100k lines, and allow the `history` command to
# access all of them rather than truncating
export HISTFILESIZE=100000

# When an interactive shell exits, the last $HISTSIZE lines in the command
# history are written to $HISTFILE
export HISTSIZE=$HISTFILESIZE

# Commands to ignore in the history file. ? and ?? ignore trival one-letter and
# two-letter commands.
export HISTIGNORE="?:??:ls:l:ll:ls -al:h:cd:vi:vim:history:,history:g:gg:g d:tmux:tmux attach"

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
,path --prepend $HOME/bin

# Similarlly, use /f/bin if it exists
if [ -d "/f/bin" ]; then ,path --prepend $HOME/bin; fi

# Pager
export LESS='-iMFXSx4R'

# Ranger shouldn't use any default config - only want my own bindings
export RANGER_LOAD_DEFAULT_RC=FALSE

# [2021-05-16] Some better bind settings
bind 'set colored-stats on'
bind 'set colored-completion-prefix on'

# Use bat if installed. Note - I have to run a bat cache command before this
# will get picked up for the first time.
export BAT_THEME="ansimatt"
export BAT_STYLE=grid,header,changes
if [ $(command -v bat) ]; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    alias cat='bat'
    alias batg='BAT_STYLE=grid,header,changes,numbers batgrep -p'
fi
