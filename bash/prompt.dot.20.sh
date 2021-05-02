# Root prompt
if [[ $DOTFILES_LITE ]]; then
    PS1="\n$ "

elif [[ $EUID == 0 ]]; then
    _user="$ANSIred$debian_chroot\u"
    _hostname="$HOST_COLOUR_ANSI\h"
    _pwd="$ANSIbase1\w"
    PS1="\n$_user $_hostname $_pwd\n$ANSIred# "

else
    # Standard prompt
    _user="$ANSIyellow$debian_chroot\u"
    _hostname="$HOST_COLOUR_ANSI\h"
    _pwd="$HOST_COLOUR_ANSI\w"

    # Add git info
    if command -v __git_ps1 >/dev/null; then
        GIT_PS1_SHOWDIRTYSTATE=true
        _git_info="$ANSIbase01\$(__git_ps1)"
    fi

    # PS1="\n$_hostname: $_pwd$_git_info\n$ANSIbase1$ "
    PS1="\n$_pwd$_git_info\n$ANSIbase1$ "
fi

# Reset prompt, so first line of output doesn't appear in weird colours (eg. git
# status output)
export PS1="$PS1$ANSIreset"

export PROMPT_DIRTRIM=4

# Write to history file on every prompt to make sure it always gets saved. Don't
# read on every prompt, as prefer to use immediate history per-shell.
export PROMPT_COMMAND="history -a;"
