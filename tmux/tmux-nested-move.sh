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

# Check if destination pane is a nested session and toggle passthrough.
# Args: $1=pane_id, $2=outer_session_name
check_nested_destination() {
    local pane="$1" session="$2"
    local target cmd state
    target=$(tmux display-message -t "$pane" -p '#{@nested}' 2>/dev/null)
    _tlog "check_nested_destination pane=$pane nested=$target"
    if [ "$target" = "on" ]; then
        cmd=$(tmux display-message -t "$pane" -p '#{pane_current_command}')
        if [ "$cmd" != "tmux" ]; then
            # Nested tmux has exited — clean up the marker and restore
            tmux set-option -t "$pane" -p -u @nested
            state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
            if [ "$state" = "on" ]; then
                _tlog "calling toggle (stale cleanup)"
                "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
                _tlog "toggle returned"
            fi
        fi
        # Auto focus-in when @auto-focus-in is enabled (toggled via prefix-Enter)
        auto=$(tmux show -t "$session" -qv @auto-focus-in 2>/dev/null)
        if [ "$auto" = "on" ]; then
            state=$(tmux show -t "$session" -qv @passthrough 2>/dev/null)
            if [ "$state" != "on" ]; then
                _tlog "calling toggle (auto-focus enter)"
                "$DOTFILES/tmux/tmux-toggle-nested.sh" "$session"
                _tlog "toggle returned"
            fi
        fi
    fi
}

outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
_tlog "outer=$outer"

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session (or not nested at all)
    current_session=$(tmux display-message -p '#S')
    old_pane=$(tmux display-message -p '#{pane_id}')
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
    new_pane=$(tmux display-message -t "$outer" -p '#{pane_id}')
    _tlog "outer moved $old_pane -> $new_pane"
    if [ "$old_pane" != "$new_pane" ]; then
        target=$(tmux display-message -t "$new_pane" -p '#{@nested}' 2>/dev/null)
        _tlog "destination nested=$target"
        if [ "$target" = "on" ]; then
            dest_cmd=$(tmux display-message -t "$new_pane" -p '#{pane_current_command}')
            if [ "$dest_cmd" = "tmux" ]; then
                auto=$(tmux show -t "$outer" -qv @auto-focus-in 2>/dev/null)
                if [ "$auto" = "on" ]; then
                    _tlog "restyle to new nested pane"
                    "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer" "$new_pane" restyle
                    _tlog "restyle returned"
                else
                    _tlog "calling toggle (edge exit, no auto-focus)"
                    "$DOTFILES/tmux/tmux-toggle-nested.sh"
                    _tlog "toggle returned"
                fi
            else
                # Stale nested — clean up and restore
                tmux set-option -t "$new_pane" -p -u @nested
                _tlog "calling toggle (edge exit, stale nested)"
                "$DOTFILES/tmux/tmux-toggle-nested.sh"
                _tlog "toggle returned"
            fi
        else
            _tlog "calling toggle (edge exit to non-nested)"
            "$DOTFILES/tmux/tmux-toggle-nested.sh"
            _tlog "toggle returned"
        fi
    else
        _tlog "calling toggle (edge exit, didn't move)"
        "$DOTFILES/tmux/tmux-toggle-nested.sh"
        _tlog "toggle returned"
    fi
    _tlog "done"
else
    tmux select-pane "-$direction"
    _tlog "inner move done"
fi
