function ,reload() {
    # Reload dotfiles environment
    source $DOTFILES/activate.sh
    bind -f $DOTFILES/inputrc.symlink
}

function ,mkd() {
    # Make and enter directory
    mkdir -p "$@" && cd "$@"
}
