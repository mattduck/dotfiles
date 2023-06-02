# Reload dotfiles environment
function ,dotfiles-reload {
    source "$DOTFILES/activate.sh"
    bind -f "$DOTFILES/inputrc.symlink"
}

# Use fzf to cd into a fasd directory.
function ,cd {
    local dir
    dir=$(fasd -dRl | fzf-tmux --header=",cd" --exact --query="$1" --preview="ls {}") && cd "${dir}" || return 1
}

# Show env
function ,env {
    env | sort | fzf --exact | tee /dev/tty | pbcopy
    echo "Copied to clipboard"
}

# show history without numbers
function ,history {
    history | awk '{$1="";print substr($0,2)}' | fzf --exact | tee /dev/tty | pbcopy
    echo "Copied to clipboard"
}

# Shows the source of a function
function ,which-function {
    shopt -s extdebug
    declare -F "$@"
    shopt -u extdebug
}
