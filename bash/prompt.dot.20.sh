# Root prompt
if [[ $DOTFILES_LITE ]]; then
    PS1="\n$ "

elif [[ $EUID == 0 ]]; then
    _user="\[$Ared\]$debian_chroot\u"
    _hostname="\[$HOST_COLOUR_ANSI\]\h"
    _pwd="\[$Abase1\]\w"
    PS1="\n$_user $_hostname $_pwd\n\[$Ared\]# "

else
    # Standard prompt
    _user="\[$Ayellow\]$debian_chroot\u"
    _hostname="\[$HOST_COLOUR_ANSI\]\h"
    _pwd="\[$HOST_COLOUR_ANSI\]\w"

    # Add git info
    if command -v __git_ps1 >/dev/null; then
        GIT_PS1_SHOWDIRTYSTATE=true
        _git_info="\[$Abase01\]\$(__git_ps1)"
    fi

    PS1="\n$_hostname: $_pwd$_git_info\n\[$Abase1\]$ "
fi

export PROMPT_DIRTRIM=4

# Write to history file on every prompt to make sure it always gets saved. Don't
# read on every prompt, as prefer to use immediate history per-shell.
export PROMPT_COMMAND="history -a;"
