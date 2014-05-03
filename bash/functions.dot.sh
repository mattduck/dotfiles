function ,reload() {
    # Reload dotfiles environment
    source $DOTFILES/activate.sh
    bind -f $DOTFILES/inputrc.symlink
}

function ,pathadd() {
    # Add current directory or givens paths to $PATH
    if [ -z "$*" ]; then
        export PATH="$PATH:$(pwd)"
    else
        for path in "$@"; do
            real_path="$(echo $(cd $(dirname $path); pwd)/$(basename $1))"
            export PATH="$PATH:$real_path"
        done
    fi
    echo "New path:\n$($DOTFILES/bin/,pathecho)"
}

function ,mkd() {
    # Make and enter directory
    mkdir -p "$@" && cd "$@"
}
