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
function ,tmux-last-layout() {
    tmux select-layout -o
}

function ,tmux-nested() {
    local outer_session
    outer_session=$(tmux display-message -p '#S')
    tmux set-option -p @nested on
    TMUX= tmux new-session "MD_TMUX_OUTER='$outer_session' bash" \; \
        set-environment MD_TMUX_OUTER "$outer_session" \; \
        set-option -w pane-border-lines single \; \
        set-option -w pane-border-status off \; \
        set-option -w pane-active-border-style "fg=colour6" \; \
        set-option -w pane-border-style "fg=colour7" \; \
        set-option status-position bottom \; \
        set-option status-justify left \; \
        set-option status-left "#[fg=colour8] #S " \; \
        set-option window-status-current-style "reverse,fg=colour6" \; \
        set-option window-status-format " #I:#W " \; \
        set-option window-status-current-format " #I:#W " \; \
        set-hook after-new-window \
            "set-option -w pane-border-lines single ; \
             set-option -w pane-border-status off ; \
             set-option -w pane-active-border-style fg=colour6 ; \
             set-option -w pane-border-style fg=colour7 ; \
             set-option window-status-current-style reverse,fg=colour6 ; \
             set-option window-status-format ' #I:#W ' ; \
             set-option window-status-current-format ' #I:#W '"

    # Inner tmux has exited — clean up
    tmux set-option -p -u @nested
    local state
    state=$(tmux show -t "$outer_session" -qv @passthrough 2>/dev/null)
    if [ "$state" = "on" ]; then
        "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer_session"
    fi
}

function ,tmux-toggle-nested() {
    $DOTFILES/tmux/tmux-toggle-nested.sh
}

function ,tmux-reset-nested() {
    tmux set -g prefix C-a
    tmux set -g @passthrough off
    tmux bind-key C-a send-prefix
    tmux set -u key-table
    tmux set -u status-style
    tmux set -g pane-active-border-style "fg=colour12"
    tmux set -g pane-border-style "fg=colour15"
    # Clear @nested from all panes
    local pane_id
    tmux list-panes -s -F '#{pane_id}' | while read pane_id; do
        tmux set-option -t "$pane_id" -p -u @nested 2>/dev/null
    done
    echo "Reset prefix to C-a, restored bindings and border styles"
}

function ,tmux-toggle-titles() {
    local current
    current=$(tmux show -v pane-border-status 2>/dev/null)
    if [[ "$current" == "top" ]]; then
        tmux set pane-border-status off
        echo "Pane borders off"
    else
        tmux set pane-border-status top
        tmux set pane-border-format "#{?pane_active,#[reverse],#[bg=colour8,fg=colour0]} #($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path}) #[default]"
        echo "Pane borders on"
    fi
}
