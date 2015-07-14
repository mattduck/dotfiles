function ,dotfiles-reload() {
    # Reload dotfiles environment
    source $DOTFILES/activate.sh
    bind -f $DOTFILES/inputrc.symlink
}

function ,mkd() {
    # Make and enter directory
    mkdir -p "$@" && cd "$@"
}

function ,history () {
    # Show timestamps, colour
    HISTTIMEFORMAT=`echo -e "\033[31m%F \033[32m%T  \033[34m"` history
}
