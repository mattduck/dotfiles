#!/bin/sh
# Toggle passthrough on the outer tmux session so keys reach the inner session.
# Reads MD_TMUX_OUTER from tmux environment, or accepts outer session name as $1.
# Optional $2 = pane ID to use for determining the active inner session (avoids
# stale active-pane resolution in background run-shell contexts).
# Optional $3 = "restyle" to only switch which inner session appears active,
# without toggling passthrough on/off. Used when moving between nested panes.
# With no args and no MD_TMUX_OUTER, forwards C-a a to the inner session (for keybind use).

#_log=/tmp/tmux-nested-debug.log
#_t0=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
#_tlog() { _now=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))'); echo "    toggle +$((_now - _t0))ms  $1" >> "$_log"; }
#echo "  toggle start $(gdate +%H:%M:%S.%3N 2>/dev/null || date +%H:%M:%S) args=$*" >> "$_log"
_tlog() { :; }

outer="${1:-}"
active_pane="${2:-}"
mode="${3:-}"

if [ -z "$outer" ]; then
    outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
fi
_tlog "resolved outer=$outer"

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session via keybind — forward C-a a to the inner session
    tmux send-keys C-a a
    exit 0
fi

# --- Restyle mode: switch active inner without toggling passthrough ---
if [ "$mode" = "restyle" ]; then
    old_inner=$(tmux show -t "$outer" -qv @_active_inner 2>/dev/null)
    if [ -n "$active_pane" ]; then
        new_inner=$(tmux display-message -t "$active_pane" -p '#{@inner_session}')
    else
        new_inner=$(tmux display-message -t "$outer" -p '#{@inner_session}')
    fi
    _tlog "restyle old=$old_inner new=$new_inner"
    if [ "$old_inner" = "$new_inner" ] || [ -z "$new_inner" ]; then
        _tlog "restyle: no change"
        exit 0
    fi
    all_windows=$(tmux list-windows -a -F '#{session_name} #{window_id}' 2>/dev/null)
    windows_for() { echo "$all_windows" | while read s w; do [ "$s" = "$1" ] && echo "$w"; done; }
    _tlog "fetched all_windows"

    cmd="set -t '$outer' @_active_inner '$new_inner'"
    # Dim old active inner
    if [ -n "$old_inner" ]; then
        old_wins=$(windows_for "$old_inner")
        if [ -n "$old_wins" ]; then
            cmd="$cmd \\; set -t '$old_inner' status-style 'bg=colour0,fg=colour8'"
            for w in $old_wins; do
                cmd="$cmd \\; set -t '$w' window-status-current-style 'dim,fg=colour6'"
                cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-lines simple"
                cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'dim,fg=colour7'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'dim,fg=colour7'"
            done
        fi
    fi
    # Brighten new active inner
    new_wins=$(windows_for "$new_inner")
    if [ -n "$new_wins" ]; then
        cmd="$cmd \\; set -t '$new_inner' status-style 'bg=colour0,fg=colour8'"
        for w in $new_wins; do
            cmd="$cmd \\; set -t '$w' window-status-current-style 'fg=colour6'"
            cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-lines double"
            cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'fg=colour15'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'fg=colour7'"
        done
    fi
    _tlog "restyle cmd built, running eval"
    eval "tmux $cmd"
    _tlog "eval done"
    exit 0
fi

# --- Normal toggle: full passthrough on/off ---

# Single list-panes call: get inner session names, pane IDs, and active flag.
# This replaces separate list-panes + display-message calls.
pane_info=$(tmux list-panes -s -t "$outer" -F '#{@inner_session}|#{pane_id}|#{pane_active}' 2>/dev/null)
inners=$(echo "$pane_info" | sed -n 's/|.*//; /./p')

# Derive active_inner from pane_info: if active_pane was explicitly provided,
# match by pane_id; otherwise use the pane marked active.
if [ -n "$active_pane" ]; then
    active_inner=$(echo "$pane_info" | while IFS='|' read inn pid act; do
        [ "$pid" = "$active_pane" ] && [ -n "$inn" ] && echo "$inn" && break
    done)
else
    active_inner=$(echo "$pane_info" | while IFS='|' read inn pid act; do
        [ "$act" = "1" ] && [ -n "$inn" ] && echo "$inn" && break
    done)
fi
_tlog "list-panes inners=$(echo $inners | tr '\n' ' ') active_inner=$active_inner"

# Fetch all windows across all sessions in one call, then filter per-inner in shell.
# This replaces N × (has-session + list-windows) with a single tmux command.
all_windows=$(tmux list-windows -a -F '#{session_name} #{window_id}' 2>/dev/null)
_tlog "fetched all_windows"

# Helper: get window IDs for a given session name from the cached list
windows_for() { echo "$all_windows" | while read s w; do [ "$s" = "$1" ] && echo "$w"; done; }

state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)
_tlog "current state=$state"

if [ "$state" = "on" ]; then
    # Restore outer — unset overrides so globals from tmux.conf take effect
    # Collect all inner window IDs first, then apply everything in one tmux call
    cmd="set -t '$outer' prefix C-a"
    cmd="$cmd \\; set -t '$outer' @passthrough off"
    cmd="$cmd \\; set -t '$outer' -u @_active_inner"
    cmd="$cmd \\; set -t '$outer' -u status-style"
    cmd="$cmd \\; set -t '$outer' -u pane-active-border-style"
    cmd="$cmd \\; set -t '$outer' -u pane-border-style"
    _tlog "building restore cmd"
    for inner in $inners; do
        inner_wins=$(windows_for "$inner")
        [ -z "$inner_wins" ] && continue
        _tlog "  restore inner=$inner"
        cmd="$cmd \\; set -t '$inner' status-style 'bg=colour0,fg=colour8'"
        for w in $inner_wins; do
            cmd="$cmd \\; set -t '$w' window-status-current-style 'dim,fg=colour6'"
            cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-lines simple"
            cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'dim,fg=colour7'"
            cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'dim,fg=colour7'"
        done
    done
    _tlog "built restore cmd, running eval"
    eval "tmux $cmd"
    _tlog "eval done"
else
    # Build one batched tmux command for outer + all inner styling
    cmd="set -t '$outer' prefix None"
    cmd="$cmd \\; set -t '$outer' @passthrough on"
    cmd="$cmd \\; set -t '$outer' @_active_inner '$active_inner'"
    cmd="$cmd \\; set -t '$outer' status-style 'fg=colour8,bg=colour0'"
    cmd="$cmd \\; set -t '$outer' pane-active-border-style 'fg=colour6'"
    cmd="$cmd \\; set -t '$outer' pane-border-style 'dim,fg=colour8'"
    _tlog "building passthrough cmd"
    for inner in $inners; do
        inner_wins=$(windows_for "$inner")
        [ -z "$inner_wins" ] && continue
        _tlog "  passthrough inner=$inner"
        cmd="$cmd \\; set -t '$inner' status-style 'bg=colour0,fg=colour8'"
        if [ "$inner" = "$active_inner" ]; then
            for w in $inner_wins; do
                cmd="$cmd \\; set -t '$w' window-status-current-style 'fg=colour6'"
                cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-lines double"
                cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'fg=colour15'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'fg=colour7'"
            done
        else
            for w in $inner_wins; do
                cmd="$cmd \\; set -t '$w' window-status-current-style 'dim,fg=colour6'"
                cmd="$cmd \\; set -t '$w' window-status-style 'fg=colour8'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-lines simple"
                cmd="$cmd \\; set-option -w -t '$w' pane-active-border-style 'dim,fg=colour7'"
                cmd="$cmd \\; set-option -w -t '$w' pane-border-style 'dim,fg=colour7'"
            done
        fi
    done
    _tlog "built passthrough cmd, running eval"
    eval "tmux $cmd"
    _tlog "eval done"
fi
