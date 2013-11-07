# Root prompt
if [[ $EUID == 0 ]]; then
    _user="\[$Ared\]$debian_chroot\u"
    _hostname="\[$HOST_COLOUR_ANSI\]\h"
    _pwd="\[$Abase1\]\w"
    PS1="$_user $_hostname $_pwd\n\[$Ared\]# "

# Standard prompt
else
    _user="\[$Ayellow\]$debian_chroot\u"
    _hostname="\[$Ayellow\]\h"
    _pwd="\[$HOST_COLOUR_ANSI\]\w"

    # Add git info
    if command -v __git_ps1 >/dev/null; then
        GIT_PS1_SHOWDIRTYSTATE=true
        _git_info="\[$Abase01\]\$(__git_ps1)"
    fi

    PS1="$_pwd$_git_info\n\[$Abase1\]$ "
fi

export PROMPT_DIRTRIM=4                 
