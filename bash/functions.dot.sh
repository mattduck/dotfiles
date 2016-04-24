function ,dotfiles-reload() {
    # Reload dotfiles environment
    source $DOTFILES/activate.sh
    bind -f $DOTFILES/inputrc.symlink
}


function ,mkd() {
    # Make and enter directory
    mkdir -p "$@" && cd "$@"
}


function ,venv-activate() {
    command -v deactivate && deactivate
    activate_script=$(locate /bin/activate | grep -e "venv" | grep -e "activate$" | fzf-tmux --query="$1" --header=",venv-activate") &&\
    source "$activate_script" &&\
    echo "Sourced $activate_script"
}


# Use fzf to cd into a fasd directory.
function ,cd () {
    local dir
    dir=$(fasd -dRl | fzf-tmux --header=",cd" --exact --query="$1") && cd "${dir}" || return 1
}


# Use fzf to edit and execute a history command with fc.
function ,hist-exec {
    local hist_number
    hist_number=$((fc -l 1 || history) | fzf --tac --query "$1" --header=",hist-exec" | awk '{print $1}') && fc "$hist_number"
}
