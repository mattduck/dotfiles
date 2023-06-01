# Root prompt
if [[ $EUID == 0 ]]; then
    _user="$ANSIred$debian_chroot\u"
    _pwd="$ANSIbrightwhite\w"
    PS1="\n$_user $_pwd\n$ANSIred# "

else
    # Standard prompt
    _user="$ANSIyellow$debian_chroot\u"
    _pwd="$ANSIblue\w"

    # Add git info
    if command -v __git_ps1 >/dev/null; then
        GIT_PS1_SHOWDIRTYSTATE=true
        _git_info="$ANSIbrightgreen\$(__git_ps1)"
    fi

    PS1="\n$_pwd$_git_info\n$ANSIbrightcyan$ "
fi

# Reset prompt, so first line of output doesn't appear in weird colours (eg. git
# status output)
export PS1="$PS1$ANSIreset"

export PROMPT_DIRTRIM=4

# Write to history file on every prompt to make sure it always gets saved. Don't
# read on every prompt, as prefer to use immediate history per-shell.
export PROMPT_COMMAND="history -a;"
