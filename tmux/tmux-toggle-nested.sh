#!/bin/sh
# Toggle passthrough on the outer tmux session so keys reach the inner session.
# Reads MD_TMUX_OUTER from tmux environment, or accepts outer session name as $1.
# Optional $2 = pane ID to use for determining the active inner session (avoids
# stale active-pane resolution in background run-shell contexts).
# With no args and no MD_TMUX_OUTER, forwards C-a a to the inner session (for keybind use).

outer="${1:-}"
active_pane="${2:-}"

if [ -z "$outer" ]; then
    outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
fi

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session via keybind — forward C-a a to the inner session
    tmux send-keys C-a a
    exit 0
fi

# Find all inner sessions from the outer panes' @inner_session option (-s for all windows)
inners=$(tmux list-panes -s -t "$outer" -F '#{@inner_session}' 2>/dev/null | grep .)

state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)

if [ "$state" = "on" ]; then
    # Restore outer — unset overrides so globals from tmux.conf take effect
    # Collect all inner window IDs first, then apply everything in one tmux call
    cmd="set -t '$outer' prefix C-a"
    cmd="$cmd \\; set -t '$outer' @passthrough off"
    cmd="$cmd \\; set -t '$outer' -u status-style"
    cmd="$cmd \\; set -t '$outer' -u pane-active-border-style"
    cmd="$cmd \\; set -t '$outer' -u pane-border-style"
    for inner in $inners; do
        tmux has-session -t "$inner" 2>/dev/null || continue
        cmd="$cmd \\; set -t '$inner' status-style 'bg=colour0,fg=colour8'"
        for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
            cmd="$cmd \\; set -t '$w' window-status-current-style 'dim,fg=colour6'"
            cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-lines simple"
            cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'dim,fg=colour7'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'dim,fg=colour7'"
        done
    done
    eval "tmux $cmd"
else
    # Active pane's inner session — use explicit pane target if provided (background
    # run-shell may not reflect the most recent select-pane)
    if [ -n "$active_pane" ]; then
        active_inner=$(tmux display-message -t "$active_pane" -p '#{@inner_session}')
    else
        active_inner=$(tmux display-message -t "$outer" -p '#{@inner_session}')
    fi
    # Collect all window IDs up front
    # Build one batched tmux command for outer + all inner styling
    cmd="set -t '$outer' prefix None"
    cmd="$cmd \\; set -t '$outer' @passthrough on"
    cmd="$cmd \\; set -t '$outer' status-style 'fg=colour8,bg=colour0'"
    cmd="$cmd \\; set -t '$outer' pane-active-border-style 'fg=colour6'"
    cmd="$cmd \\; set -t '$outer' pane-border-style 'dim,fg=colour8'"
    for inner in $inners; do
        tmux has-session -t "$inner" 2>/dev/null || continue
        cmd="$cmd \\; set -t '$inner' status-style 'bg=colour0,fg=colour8'"
        if [ "$inner" = "$active_inner" ]; then
            for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
                cmd="$cmd \\; set -t '$w' window-status-current-style 'fg=colour6'"
                cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-lines double"
                cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'fg=colour15'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'fg=colour7'"
            done
        else
            for w in $(tmux list-windows -t "$inner" -F '#{window_id}'); do
                cmd="$cmd \\; set -t '$w' window-status-current-style 'dim,fg=colour6'"
                cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-lines simple"
                cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'dim,fg=colour7'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'dim,fg=colour7'"
            done
        fi
    done
    eval "tmux $cmd"
fi
