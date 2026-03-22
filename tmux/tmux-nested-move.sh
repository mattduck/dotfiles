#!/bin/sh
# Move pane with nested session awareness.
# - In nested (inner) session at edge: toggle passthrough off, move in outer
# - In outer session moving into a @nested pane: toggle passthrough on
# Usage: tmux-nested-move.sh <direction> (L/D/U/R)

direction="$1"

# Check if destination pane is a nested session and toggle passthrough.
# Args: $1=pane_id, $2=outer_session_name
check_nested_destination() {
    local pane="$1" session="$2"
    local target cmd state
    target=$(tmux display-message -t "$pane" -p '#{@nested}' 2>/dev/null)
    if [ "$target" = "on" ]; then
        cmd=$(tmux display-message -t "$pane" -p '#{pane_current_command}')
        if [ "$cmd" != "tmux" ]; then
            # Nested tmux has exited — clean up the marker and restore
            tmux set-option -t "$pane" -p -u @nested
            state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
            if [ "$state" = "on" ]; then
                "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
            fi
        fi
        # Auto focus-in to nested tmux panes is disabled; use C-a a or C-a o
    fi
}

outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session (or not nested at all)
    current_session=$(tmux display-message -p '#S')
    old_pane=$(tmux display-message -p '#{pane_id}')
    tmux select-pane "-$direction"
    new_pane=$(tmux display-message -t "$current_session" -p '#{pane_id}')
    if [ "$old_pane" != "$new_pane" ]; then
        check_nested_destination "$new_pane" "$current_session"
    fi
    exit 0
fi

# We're in the inner session — check if at edge
case "$direction" in
    L) at_edge=$(tmux display-message -p '#{pane_at_left}') ;;
    R) at_edge=$(tmux display-message -p '#{pane_at_right}') ;;
    U) at_edge=$(tmux display-message -p '#{pane_at_top}') ;;
    D) at_edge=$(tmux display-message -p '#{pane_at_bottom}') ;;
esac

if [ "$at_edge" = "1" ]; then
    # Toggle passthrough off and move in outer session
    "$DOTFILES/tmux/tmux-toggle-nested.sh"
    old_pane=$(tmux display-message -t "$outer" -p '#{pane_id}')
    tmux select-pane -t "$outer" "-$direction"
    new_pane=$(tmux display-message -t "$outer" -p '#{pane_id}')
    if [ "$old_pane" != "$new_pane" ]; then
        check_nested_destination "$new_pane" "$outer"
    fi
else
    tmux select-pane "-$direction"
fi
