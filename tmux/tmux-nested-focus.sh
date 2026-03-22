#!/bin/sh
# Sync passthrough state when a pane gains focus (via mouse, app switch, etc).
# Runs via run-shell -b from mouse binding with a sleep so that option
# changes take effect after tmux finishes processing the mouse event.
# Arguments: $1=session_name $2=pane_id (captured at binding time)

sleep 0.05

session="$1"
pane="$2"

if [ -z "$session" ] || [ -z "$pane" ]; then
    exit 0
fi

nested=$(tmux display-message -t "$pane" -p '#{@nested}' 2>/dev/null)

if [ "$nested" = "on" ]; then
    cmd=$(tmux display-message -t "$pane" -p '#{pane_current_command}')
    if [ "$cmd" = "tmux" ]; then
        state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
        if [ "$state" != "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
        fi
    else
        tmux set-option -t "$pane" -p -u @nested
        state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
        if [ "$state" = "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
        fi
    fi
else
    state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
    if [ "$state" = "on" ]; then
        "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
    fi
fi
