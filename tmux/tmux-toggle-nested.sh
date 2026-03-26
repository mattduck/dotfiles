#!/bin/sh
# Toggle passthrough on the outer tmux session so keys reach the inner session.
# Used by the prefix-a keybind for manual passthrough toggle.
# Most passthrough operations are inlined in tmux-nested-move.sh and
# tmux-nested-focus.sh for speed — this script handles the manual keybind
# and restyle mode when called from focus.sh.
#
# With no args and no MD_TMUX_OUTER, forwards C-a a to the inner session.
# Optional $1 = outer session name, $2 = pane ID, $3 = "restyle".

_log=/tmp/tmux-nested-debug.log
_t0=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
_tlog() { _now=$(gdate +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))'); echo "    toggle +$((_now - _t0))ms  $1" >> "$_log"; }
echo "  toggle start $(gdate +%H:%M:%S.%3N 2>/dev/null || date +%H:%M:%S) args=$*" >> "$_log"

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

# --- Restyle mode: update which inner session is active ---
if [ "$mode" = "restyle" ]; then
    if [ -n "$active_pane" ]; then
        new_inner=$(tmux display-message -t "$active_pane" -p '#{@inner_session}')
    else
        new_inner=$(tmux display-message -t "$outer" -p '#{@inner_session}')
    fi
    _tlog "restyle new=$new_inner"
    if [ -n "$new_inner" ]; then
        tmux set -t "$outer" @_active_inner "$new_inner"
    fi
    _tlog "done"
    exit 0
fi

# --- Normal toggle: passthrough on/off ---
if [ -n "$active_pane" ]; then
    active_inner=$(tmux display-message -t "$active_pane" -p '#{@inner_session}' 2>/dev/null)
else
    active_inner=$(tmux display-message -t "$outer" -p '#{@inner_session}' 2>/dev/null)
fi
state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)
_tlog "state=$state active_inner=$active_inner"

if [ "$state" = "on" ]; then
    _tlog "exit passthrough"
    auto=$(tmux show -t "$outer" -qv @auto-focus-in 2>/dev/null)
    [ -n "$active_inner" ] && tmux set -t "$active_inner" -u @outer_passthrough 2>/dev/null &
    if [ "$auto" = "on" ]; then
        eval "tmux set -t '$outer' prefix C-a \
            \\; set -t '$outer' @passthrough off \
            \\; set -t '$outer' -u @_active_inner"
    else
        eval "tmux set -t '$outer' prefix C-a \
            \\; set -t '$outer' @passthrough off \
            \\; set -t '$outer' -u @_active_inner \
            \\; set -t '$outer' -u pane-border-style"
    fi
    _tlog "eval done"
else
    _tlog "enter passthrough"
    [ -n "$active_inner" ] && tmux set -t "$active_inner" @outer_passthrough on 2>/dev/null &
    eval "tmux set -t '$outer' prefix None \
        \\; set -t '$outer' @passthrough on \
        \\; set -t '$outer' @_active_inner '$active_inner' \
        \\; set -t '$outer' pane-border-style 'fg=colour8,dim'"
    _tlog "eval done"
fi
