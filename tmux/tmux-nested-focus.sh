#!/bin/sh
# Sync passthrough state when a pane gains focus (via mouse, app switch, etc).
# Runs via run-shell -b from mouse binding with a sleep so that option
# changes take effect after tmux finishes processing the mouse event.
# Arguments: $1=session_name $2=pane_id (from format expansion in the caller —
# for mouse bindings these resolve in the mouse-target pane context).

sleep 0.05

session="$1"
pane="$2"

if [ -z "$session" ] || [ -z "$pane" ]; then
    exit 0
fi

nested=$(tmux display-message -t "$pane" -p '#{@nested}' 2>/dev/null)

if [ "$nested" = "on" ]; then
    cmd=$(tmux display-message -t "$pane" -p '#{pane_current_command}')
    if [ "$cmd" != "tmux" ]; then
        # Nested tmux has exited — clean up marker and restore passthrough
        tmux set-option -t "$pane" -p -u @nested
        state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
        if [ "$state" = "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
        fi
    fi
    # Auto focus-in when @auto-focus-in is enabled (toggled via prefix-Enter)
    auto=$(tmux show -t "$session" -qv @auto-focus-in 2>/dev/null)
    if [ "$auto" = "on" ]; then
        state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
        if [ "$state" != "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session" "$pane"
        else
            # Passthrough already on but we switched to a different nested pane.
            # Re-apply styling so the new active inner session gets bright borders
            # and the old one dims. Reset state so toggle re-enters the "on" branch.
            tmux set -t "$session" @passthrough off
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session" "$pane"
        fi
    fi
else
    state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
    if [ "$state" = "on" ]; then
        "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
    fi
fi
