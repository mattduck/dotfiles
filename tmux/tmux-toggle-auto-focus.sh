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
    # We're in the outer session
    outer=$(tmux display-message -p '#S')
    auto=$(tmux show -t "$outer" -qv @auto-focus-in 2>/dev/null)
    if [ "$auto" = "on" ]; then
        # Turn off auto-focus from outer session
        tmux set -t "$outer" @auto-focus-in off
        state=$(tmux show -t "$outer" -qv @passthrough 2>/dev/null)
        if [ "$state" = "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer"
        else
            tmux set -t "$outer" -u pane-border-style
        fi
    else
        # Turn on auto-focus from outer session
        tmux set -t "$outer" @auto-focus-in on
        tmux set -t "$outer" pane-border-style 'fg=colour8,dim'
        # If current pane is nested, enter passthrough too
        pane_info=$(tmux display-message -p '#{@nested}|#{pane_current_command}')
        nested="${pane_info%%|*}"
        cmd="${pane_info#*|}"
        if [ "$nested" = "on" ] && [ "$cmd" = "tmux" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer"
        fi
    fi
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
