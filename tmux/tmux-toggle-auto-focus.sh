#!/bin/sh
# Toggle auto-focus-in mode on the outer session. When enabled, clicking or
# focusing a nested pane automatically enters passthrough. Also toggles
# passthrough to match, so pressing prefix-Enter both enters/exits the inner
# session and sets whether future focus events should do the same.

outer="${1:-}"
if [ -z "$outer" ]; then
    outer=$(tmux show-environment MD_TMUX_OUTER 2>/dev/null | sed 's/^MD_TMUX_OUTER=//')
fi

if [ -z "$outer" ] || echo "$outer" | grep -q '^-'; then
    # We're in the outer session — forward to inner session
    tmux send-keys C-a Enter
    exit 0
fi

# Set auto-focus-in to opposite of current passthrough, then toggle to match
state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)
if [ "$state" = "on" ]; then
    tmux set -t "$outer" @auto-focus-in off
else
    tmux set -t "$outer" @auto-focus-in on
fi

"$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer"
