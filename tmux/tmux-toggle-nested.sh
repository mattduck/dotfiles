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

# Find all inner sessions (have MD_TMUX_OUTER set to our outer name)
inners=""
for s in $(tmux list-sessions -F '#{session_name}'); do
    val=$(tmux show-environment -t "$s" MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
    if [ "$val" = "$outer" ]; then
        inners="$inners $s"
    fi
done

state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)

if [ "$state" = "on" ]; then
    # Restore outer — unset overrides so globals from tmux.conf take effect
    tmux set -t "$outer" prefix C-a \; \
        set -t "$outer" @passthrough off
    tmux set -t "$outer" -u status-style
    tmux set -t "$outer" -u pane-active-border-style
    tmux set -t "$outer" -u pane-border-style
    # Dim inner sessions
    for inner in $inners; do
        tmux set -t "$inner" status-style "bg=colour0,fg=colour8"
        for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
            tmux set -t "$w" window-status-current-style "dim,fg=colour6"
            tmux set -t "$w" window-status-style "fg=colour8"
            tmux set-option -w -t "$w" pane-border-lines simple
            tmux set-option -w -t "$w" pane-active-border-style "dim,fg=colour7"
            tmux set-option -w -t "$w" pane-border-style "dim,fg=colour7"
        done
    done
else
    # Dim outer
    tmux set -t "$outer" prefix None \; \
        set -t "$outer" @passthrough on
    tmux set -t "$outer" status-style "fg=colour8,bg=colour0"
    tmux set -t "$outer" pane-active-border-style "fg=colour6"
    tmux set -t "$outer" pane-border-style "fg=colour8"
    # Highlight the active inner session, keep others dimmed
    active_tty=$(tmux display-message -t "$outer" -p '#{pane_tty}')
    active_inner=$(tmux list-clients -F '#{client_tty} #{session_name}' | \
        awk -v tty="$active_tty" '$1 == tty {print $2}')
    for inner in $inners; do
        if [ "$inner" = "$active_inner" ]; then
            tmux set -t "$inner" status-style "bg=colour0,fg=colour8"
            for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
                tmux set -t "$w" window-status-current-style "fg=colour6"
                tmux set -t "$w" window-status-style "fg=colour8"
                tmux set-option -w -t "$w" pane-border-lines single
                tmux set-option -w -t "$w" pane-active-border-style "fg=colour15"
                tmux set-option -w -t "$w" pane-border-style "fg=colour7"
            done
        else
            tmux set -t "$inner" status-style "bg=colour0,fg=colour8"
            for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
                tmux set -t "$w" window-status-current-style "dim,fg=colour6"
                tmux set -t "$w" window-status-style "fg=colour8"
                tmux set-option -w -t "$w" pane-border-lines simple
                tmux set-option -w -t "$w" pane-active-border-style "dim,fg=colour7"
                tmux set-option -w -t "$w" pane-border-style "dim,fg=colour7"
            done
        fi
    done
fi
