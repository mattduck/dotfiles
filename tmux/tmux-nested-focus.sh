#!/bin/sh
# Sync passthrough state when a pane gains focus (via mouse, app switch, etc).
# Called from:
#   MouseDown1Pane binding — passes only session name ($1); pane is queried
#     via list-panes after select-pane -t= has completed.
#   client-focus-in hook — passes session name ($1) and pane_id ($2) directly,
#     since #{pane_id} is correct in hook context.

sleep 0.05

session="$1"
pane="${2:-}"

# If pane not provided (mouse binding), query the session's active pane.
# list-panes queries server state directly, unaffected by run-shell -b context
# (unlike display-message -p and #{pane_id} which return stale values).
if [ -z "$pane" ]; then
    pane=$(tmux list-panes -t "$session" -f '#{pane_active}' -F '#{pane_id}' 2>/dev/null | head -1)
fi

if [ -z "$session" ] || [ -z "$pane" ]; then
    exit 0
fi

# Deduplicate: both MouseDown1Pane and client-focus-in may fire for the same
# click. Skip if we already processed this pane.
last=$(tmux show -t "$session" -qv @_last_focus_pane 2>/dev/null)
if [ "$pane" = "$last" ]; then
    exit 0
fi
tmux set -t "$session" -q @_last_focus_pane "$pane"

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
        exit 0
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
    else
        # Auto-focus off: exit passthrough when clicking away from the
        # pane where it was manually enabled.
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
