# If this shell is running within tmux, add some extra utilities

if [[ -z $TMUX ]]; then return; fi

echo "Setting up tmux environment..."

tmux source-file $DOTFILES/splitscreen/tmux.conf

function ,tminit() {
    # Setup session with a generic layout
    if [[ ! -z "$CUSTOM_TMUX_INIT" ]]; then
        echo "This session has already been initialised"
        return 1
    fi

    ORIG_WINDOW=`tmux display-message -p "#I"`

    # Usually only have one session per machine
    tmux rename-session "main"

    # Probably want a few windows
    tmux new-window
    tmux new-window

    # Go back to the original window
    tmux select-window -t $ORIG_WINDOW

    export CUSTOM_TMUX_INIT=true
    echo "Generic tmux layout initiated"
}

function ,tmreload() {
    # Reload tmux config and this file. Has to be done per pane.
    tmux source-file ~/.tmux.conf
    source $DOTFILES/tmux/tmux_env.dot.sh
    echo "Reloaded ~/.tmux.conf and $DOTFILES/tmux/tmux_env.sh"
}
