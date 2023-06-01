function ,dotfiles-reload {
    # Reload dotfiles environment
    source "$DOTFILES/activate.sh"
    bind -f "$DOTFILES/inputrc.symlink"
}

# Use fzf to cd into a fasd directory.
function ,cd {
    local dir
    dir=$(fasd -dRl | fzf-tmux --header=",cd" --exact --query="$1" --preview="ls {}") && cd "${dir}" || return 1
}

function ,env {
    # Show env
    env | sort | fzf --exact | tee /dev/tty | pbcopy
    echo "Copied to clipboard"
}

function ,history {
    # show history without numbers
    history | awk '{$1="";print substr($0,2)}' | fzf --exact | tee /dev/tty | pbcopy
    echo "Copied to clipboard"
}
