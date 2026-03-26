#!/bin/sh
# Move pane with nested session awareness.
# - In nested (inner) session at edge: toggle passthrough off, move in outer
# - In outer session moving into a @nested pane: toggle passthrough on
# Usage: tmux-nested-move.sh <direction> (L/D/U/R)

_log=/tmp/tmux-nested-debug.log
_t0=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
_tlog() { _now=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))'); echo "  move +$((_now - _t0))ms  $1" >> "$_log"; }
echo "--- move $(gdate +%H:%M:%S.%3N 2>/dev/null || date +%H:%M:%S) direction=$1 ---" >> "$_log"

direction="$1"

# Inline passthrough enter: metadata + outer active border colour
_enter_passthrough() {
    local session="$1" inner="$2"
    _tlog "inline enter passthrough inner=$inner"
    eval "tmux set -t '$session' prefix None \
        \\; set -t '$session' @passthrough on \
        \\; set -t '$session' @_active_inner '$inner' \
        \\; set -t '$session' pane-active-border-style 'fg=colour6'"
    _tlog "enter done"
}

# Inline passthrough exit: restore outer defaults
_exit_passthrough() {
    local session="$1"
    _tlog "inline exit passthrough"
    eval "tmux set -t '$session' prefix C-a \
        \\; set -t '$session' @passthrough off \
        \\; set -t '$session' -u @_active_inner \
        \\; set -t '$session' -u status-style \
        \\; set -t '$session' -u pane-active-border-style \
        \\; set -t '$session' -u pane-border-style"
    _tlog "exit done"
}

# Inline restyle: just update which inner is active
_restyle() {
    local session="$1" new_pane="$2"
    local new_inner
    new_inner=$(tmux display-message -t "$new_pane" -p '#{@inner_session}')
    _tlog "inline restyle new=$new_inner"
    if [ -n "$new_inner" ]; then
        tmux set -t "$session" @_active_inner "$new_inner"
    fi
    _tlog "restyle done"
}

# Check if destination pane is a nested session and toggle passthrough.
# Args: $1=pane_id, $2=outer_session_name
check_nested_destination() {
    local pane="$1" session="$2"
    local pane_info target cmd sess_info auto state
    # Batch pane queries: @nested and pane_current_command in one call
    pane_info=$(tmux display-message -t "$pane" -p '#{@nested}|#{pane_current_command}' 2>/dev/null)
    target="${pane_info%%|*}"
    cmd="${pane_info#*|}"
    _tlog "check_nested_destination pane=$pane nested=$target cmd=$cmd"
    if [ "$target" = "on" ]; then
        if [ "$cmd" != "tmux" ]; then
            # Nested tmux has exited — clean up the marker and restore
            tmux set-option -t "$pane" -p -u @nested
            state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
            if [ "$state" = "on" ]; then
                _tlog "exit passthrough (stale cleanup)"
                _exit_passthrough "$session"
            fi
        fi
        # Batch session queries: @auto-focus-in and @passthrough in one call
        sess_info=$(tmux display-message -t "$session" -p '#{@auto-focus-in}|#{@passthrough}' 2>/dev/null)
        auto="${sess_info%%|*}"
        state="${sess_info#*|}"
        if [ "$auto" = "on" ] && [ "$state" != "on" ]; then
            local inner
            inner=$(tmux display-message -t "$pane" -p '#{@inner_session}' 2>/dev/null)
            _tlog "enter passthrough (auto-focus)"
            _enter_passthrough "$session" "$inner"
        fi
    fi
}

outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
_tlog "outer=$outer"

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session (or not nested at all)
    # Batch session name and pane ID in one call
    _info=$(tmux display-message -p '#S|#{pane_id}')
    current_session="${_info%%|*}"
    old_pane="${_info#*|}"
    _tlog "outer session: select-pane -$direction from $old_pane"
    tmux select-pane "-$direction"
    new_pane=$(tmux display-message -t "$current_session" -p '#{pane_id}')
    _tlog "moved to $new_pane"
    if [ "$old_pane" != "$new_pane" ]; then
        check_nested_destination "$new_pane" "$current_session"
    fi
    _tlog "done"
    exit 0
fi

# We're in the inner session — check if at edge
case "$direction" in
    L) at_edge=$(tmux display-message -p '#{pane_at_left}') ;;
    R) at_edge=$(tmux display-message -p '#{pane_at_right}') ;;
    U) at_edge=$(tmux display-message -p '#{pane_at_top}') ;;
    D) at_edge=$(tmux display-message -p '#{pane_at_bottom}') ;;
esac
_tlog "inner session: at_edge=$at_edge"

if [ "$at_edge" = "1" ]; then
    # Move in outer session first, then decide how to handle passthrough.
    # This avoids a double-toggle (restore + re-enter) when moving between
    # nested panes — instead we use a single "restyle" operation.
    old_pane=$(tmux display-message -t "$outer" -p '#{pane_id}')
    tmux select-pane -t "$outer" "-$direction"
    # Batch post-move queries: new pane ID, @nested, pane_current_command, @auto-focus-in
    _post=$(tmux display-message -t "$outer" -p '#{pane_id}|#{@nested}|#{pane_current_command}|#{@auto-focus-in}')
    new_pane="${_post%%|*}"; _rest="${_post#*|}"
    target="${_rest%%|*}"; _rest="${_rest#*|}"
    dest_cmd="${_rest%%|*}"; auto="${_rest#*|}"
    _tlog "outer moved $old_pane -> $new_pane nested=$target cmd=$dest_cmd auto=$auto"
    if [ "$old_pane" != "$new_pane" ]; then
        if [ "$target" = "on" ]; then
            if [ "$dest_cmd" = "tmux" ]; then
                if [ "$auto" = "on" ]; then
                    _restyle "$outer" "$new_pane"
                else
                    _exit_passthrough "$outer"
                fi
            else
                # Stale nested — clean up and restore
                tmux set-option -t "$new_pane" -p -u @nested
                _exit_passthrough "$outer"
            fi
        else
            _exit_passthrough "$outer"
        fi
    else
        _exit_passthrough "$outer"
    fi
    _tlog "done"
else
    tmux select-pane "-$direction"
    _tlog "inner move done"
fi
