#!/bin/sh
# Sync passthrough state when a pane gains focus (via mouse, app switch, etc).
# Called from:
#   MouseDown1Pane binding — passes only session name ($1); pane is queried
#     via list-panes after select-pane -t= has completed.
#   client-focus-in hook — passes session name ($1) and pane_id ($2) directly,
#     since #{pane_id} is correct in hook context.

_log=/tmp/tmux-nested-debug.log
_t0=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
_tlog() { _now=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))'); echo "  +$((_now - _t0))ms  $1" >> "$_log"; }
echo "--- focus $(gdate +%H:%M:%S.%3N 2>/dev/null || date +%H:%M:%S) session=$1 pane=${2:-<mouse>} ---" >> "$_log"

# Inline passthrough enter: metadata + outer active border colour
_enter_passthrough() {
    local session="$1" inner="$2"
    _tlog "inline enter passthrough inner=$inner"
    [ -n "$inner" ] && tmux set -t "$inner" @outer_passthrough on 2>/dev/null &
    eval "tmux set -t '$session' prefix None \
        \\; set -t '$session' @passthrough on \
        \\; set -t '$session' @_active_inner '$inner' \
        \\; set -t '$session' pane-border-style 'fg=colour8,dim'"
    _tlog "enter done"
}

# Inline passthrough exit: restore outer defaults
_exit_passthrough() {
    local session="$1"
    local _info old_inner auto
    _info=$(tmux display-message -t "$session" -p '#{@_active_inner}|#{@auto-focus-in}' 2>/dev/null)
    old_inner="${_info%%|*}"
    auto="${_info#*|}"
    _tlog "inline exit passthrough old_inner=$old_inner auto=$auto"
    [ -n "$old_inner" ] && tmux set -t "$old_inner" -u @outer_passthrough 2>/dev/null &
    if [ "$auto" = "on" ]; then
        eval "tmux set -t '$session' prefix C-a \
            \\; set -t '$session' @passthrough off \
            \\; set -t '$session' -u @_active_inner"
    else
        eval "tmux set -t '$session' prefix C-a \
            \\; set -t '$session' @passthrough off \
            \\; set -t '$session' -u @_active_inner \
            \\; set -t '$session' -u pane-border-style"
    fi
    _tlog "exit done"
}

# Inline restyle: just update which inner is active
_restyle() {
    local session="$1" pane="$2"
    local old_inner new_inner
    old_inner=$(tmux show -t "$session" -qv @_active_inner 2>/dev/null)
    new_inner=$(tmux display-message -t "$pane" -p '#{@inner_session}')
    _tlog "inline restyle old=$old_inner new=$new_inner"
    [ -n "$old_inner" ] && [ "$old_inner" != "$new_inner" ] && tmux set -t "$old_inner" -u @outer_passthrough 2>/dev/null &
    [ -n "$new_inner" ] && tmux set -t "$new_inner" @outer_passthrough on 2>/dev/null &
    if [ -n "$new_inner" ]; then
        tmux set -t "$session" @_active_inner "$new_inner"
    fi
    _tlog "restyle done"
}

session="$1"
pane="${2:-}"

# If pane not provided (mouse binding), query the session's active pane.
# list-panes queries server state directly, unaffected by run-shell -b context
# (unlike display-message -p and #{pane_id} which return stale values).
# Short sleep gives select-pane -t= time to register before we query.
if [ -z "$pane" ]; then
    sleep 0.02
    _tlog "after sleep 0.02 (mouse path)"
    pane=$(tmux list-panes -t "$session" -f '#{pane_active}' -F '#{pane_id}' 2>/dev/null | head -1)
    _tlog "resolved pane=$pane via list-panes"
else
    _tlog "pane provided (hook path, no sleep)"
fi

if [ -z "$session" ] || [ -z "$pane" ]; then
    _tlog "exit: empty session or pane"
    exit 0
fi

# Deduplicate: both MouseDown1Pane and client-focus-in may fire for the same
# click. Skip if we already processed this pane.
last=$(tmux show -t "$session" -qv @_last_focus_pane 2>/dev/null)
if [ "$pane" = "$last" ]; then
    _tlog "exit: deduplicated (pane=$pane = last)"
    exit 0
fi
tmux set -t "$session" -q @_last_focus_pane "$pane"
_tlog "after dedup check + set last_focus_pane"

# Batch pane-level queries: @nested and pane_current_command in one call
pane_info=$(tmux display-message -t "$pane" -p '#{@nested}|#{pane_current_command}' 2>/dev/null)
nested="${pane_info%%|*}"
pane_cmd="${pane_info#*|}"
_tlog "pane_info nested=$nested cmd=$pane_cmd"

# Batch session-level queries: @auto-focus-in and @passthrough in one call
sess_info=$(tmux display-message -t "$session" -p '#{@auto-focus-in}|#{@passthrough}' 2>/dev/null)
auto="${sess_info%%|*}"
state="${sess_info#*|}"
_tlog "sess_info auto=$auto passthrough=$state"

if [ "$nested" = "on" ]; then
    if [ "$pane_cmd" != "tmux" ]; then
        # Nested tmux has exited — clean up marker and restore passthrough
        tmux set-option -t "$pane" -p -u @nested
        if [ "$state" = "on" ]; then
            _tlog "exit passthrough (stale nested cleanup)"
            _exit_passthrough "$session"
        fi
        _tlog "exit: stale nested cleanup"
        exit 0
    fi
    if [ "$auto" = "on" ]; then
        if [ "$state" != "on" ]; then
            inner=$(tmux display-message -t "$pane" -p '#{@inner_session}' 2>/dev/null)
            _tlog "enter passthrough (auto-focus)"
            _enter_passthrough "$session" "$inner"
        else
            # Passthrough already on but we switched to a different nested pane.
            _tlog "restyle (auto-focus switch)"
            _restyle "$session" "$pane"
        fi
    else
        # Auto-focus off: exit passthrough when clicking away from the
        # pane where it was manually enabled.
        if [ "$state" = "on" ]; then
            _tlog "exit passthrough (manual)"
            _exit_passthrough "$session"
        fi
    fi
else
    if [ "$state" = "on" ]; then
        _tlog "exit passthrough (focus non-nested pane)"
        _exit_passthrough "$session"
    fi
fi
_tlog "done"
