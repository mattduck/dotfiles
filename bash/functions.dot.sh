function ,dotfiles-reload() {
    # Reload dotfiles environment
    source $DOTFILES/activate.sh
    bind -f $DOTFILES/inputrc.symlink
}

function ,mkd() {
    # Make and enter directory
    mkdir -p "$@" && cd "$@"
}

function ,ag() {
    # Display coloured ag results via less.
    ag $@ \
        --color \
        --break \
        --context=2 \
        --pager="less -r"
}
