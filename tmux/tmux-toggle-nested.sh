#!/bin/sh
# Toggle passthrough on the outer tmux session so keys reach the inner session.
# Reads MD_TMUX_OUTER from tmux environment, or accepts outer session name as $1.
# With no args and no MD_TMUX_OUTER, forwards C-a a to the inner session (for keybind use).

outer="${1:-}"

if [ -z "$outer" ]; then
    outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
fi

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session via keybind — forward C-a a to the inner session
    tmux send-keys C-a a
    exit 0
fi

# Find the inner session (has MD_TMUX_OUTER set to our outer name)
inner=""
for s in $(tmux list-sessions -F '#{session_name}'); do
    val=$(tmux show-environment -t "$s" MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
    if [ "$val" = "$outer" ]; then
        inner="$s"
        break
    fi
done

state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)

if [ "$state" = "on" ]; then
    # Restore outer
    tmux set -t "$outer" prefix C-a \; \
        set -t "$outer" @passthrough off
    tmux set -t "$outer" status-style "fg=colour7,bg=colour0"
    tmux set -t "$outer" pane-active-border-style "fg=colour12"
    tmux set -t "$outer" pane-border-style "fg=colour15"
    # Dim inner status bar
    if [ -n "$inner" ]; then
        tmux set -t "$inner" status-style "bg=colour0,fg=colour8"
        tmux set -t "$inner" window-status-current-style "reverse,fg=colour6"
        tmux set -t "$inner" window-status-style "fg=colour8"
        for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
            tmux set -t "$w" window-status-current-style "reverse,fg=colour6"
            tmux set -t "$w" window-status-style "fg=colour8"
        done
    fi
else
    # Dim outer
    tmux set -t "$outer" prefix None \; \
        set -t "$outer" @passthrough on
    tmux set -t "$outer" status-style "fg=colour8,bg=colour0"
    tmux set -t "$outer" pane-active-border-style "fg=colour6"
    tmux set -t "$outer" pane-border-style "dim,fg=colour15"
    # Highlight inner status bar
    if [ -n "$inner" ]; then
        tmux set -t "$inner" status-style "bg=colour6,fg=colour0"
        tmux set -t "$inner" window-status-current-style "bg=colour0,fg=colour6"
        tmux set -t "$inner" window-status-style "fg=colour0"
        for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
            tmux set -t "$w" window-status-current-style "bg=colour0,fg=colour6"
            tmux set -t "$w" window-status-style "fg=colour0"
        done
    fi
fi
