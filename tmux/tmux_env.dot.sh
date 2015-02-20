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

    # 4 panes
    tmux splitw -h -p 50
    tmux selectp -t 1
    tmux splitw -v -p 60
    tmux selectp -t 0
    tmux splitw -v -p 60
    tmux select-layout tiled

    tmux new-window

    # Go back to the original window
    tmux select-window -t $ORIG_WINDOW

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

function ,tm-solarized-dark() {
    tmux source-file $DOTFILES/solarized/various/tmux/tmuxcolors-256.conf
}
function ,tm-solarized-light() {
    tmux source-file $DOTFILES/solarized/various/tmux/tmuxcolors-light.conf
}

