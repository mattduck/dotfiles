# Root prompt
if [[ $EUID == 0 ]]; then
    _user="\[$Ered\]$debian_chroot\u"
    _hostname="\[$HOSTCOLOUR\]\h"
    _pwd="\[$Ecyan\]\w"
    PS1="$_user $_hostname $_pwd\n\[$Ered\]# "

# Standard prompt
else
    _user="\[$Egreen\]$debian_chroot\u"
    _hostname="\[$Egreen\]\h"
    _pwd="\[$Ecyan\]\w"

    # Add git info
    if command -v __git_ps1 >/dev/null; then
        GIT_PS1_SHOWDIRTYSTATE=true
        _git_info="\[$EIgreen\]\$(__git_ps1)"
    fi

    PS1="$_user@$_hostname $_pwd$_git_info\n\[$EIcyan\]$ "
fi

export PROMPT_DIRTRIM=4                 
