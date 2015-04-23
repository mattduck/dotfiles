# If this shell is running within tmux, add some extra utilities

if [[ -z $TMUX ]]; then return; fi

echo "Setting up tmux environment..."

tmux source-file $DOTFILES/splitscreen/tmux.conf

function ,tm-init() {
    # Setup session with a generic layout
    if [[ ! -z "$CUSTOM_TMUX_INIT" ]]; then
        echo "This session has already been initialised"
        return 1
    fi

    ORIG_WINDOW=`tmux display-message -p "#I"`

    # Panes
    ,tm-new-win-vertical

    # Usually only have one session per machine
    tmux rename-session "main"

    export CUSTOM_TMUX_INIT=true
    echo "Generic tmux layout initiated"
}

function ,tm-reload() {
    # Reload tmux config and this file. Has to be done per pane.
    tmux source-file ~/.tmux.conf
    source $DOTFILES/tmux/tmux_env.dot.sh
    echo "Reloaded ~/.tmux.conf and $DOTFILES/tmux/tmux_env.sh"
}

function ,tm-new-win-vertical() {
    tmux new-window
    tmux splitw -v -p 70

    tmux select-pane -t 1
    tmux splitw -h -p 33
    tmux select-pane -t 1
    tmux splitw -h -p 50
    tmux select-pane -t 4 # main bottom pane
}

function ,tm-new-win-horizontal() {
    tmux new-window
    tmux splitw -h -p 30
    tmux splitw -v -p 50
    tmux select-pane -t 1 # main left pane
}

function ,tm-move() {
    tmux move-pane -t $1
}

function ,tm-pop() {
    tmux break-pane
}
