# If this shell is running within tmux, add some extra utilities
if [[ -z $TMUX ]]; then return; fi

function ,tmux-reload() {
    # Reload tmux config and this file. Has to be done per pane.
    tmux source-file ~/.tmux.conf
    source $DOTFILES/tmux/tmux_env.dot.sh
    echo "Reloaded ~/.tmux.conf and $DOTFILES/tmux/tmux_env.sh"
}


# Show titles on panes, to help keep track when lots of TUI-like
# programs are running
function ,tmux-toggle-titles() {
    local current
    current=$(tmux show -gv pane-border-status 2>/dev/null)
    if [[ "$current" == "top" ]]; then
        tmux set -g pane-border-status off
        echo "Pane borders off"
    else
        tmux set -g pane-border-status top
        tmux set -g pane-border-format " #(echo #{pane_current_path} | sed s\|$HOME\|~\|) "
        echo "Pane borders on"
    fi
}
