# If this shell is running within tmux, add some extra utilities
if [[ -z $TMUX ]]; then return; fi

function ,tmux-reload() {
    # Reload tmux config and this file. Has to be done per pane.
    tmux source-file ~/.tmux.conf
    source $DOTFILES/tmux/tmux_env.dot.sh
    echo "Reloaded ~/.tmux.conf and $DOTFILES/tmux/tmux_env.sh"
}
