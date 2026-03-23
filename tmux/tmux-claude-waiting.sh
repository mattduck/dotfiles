#!/bin/sh
# Set or clear the @claude-waiting pane option. The pane-border-format
# in tmux_env.dot.sh reads this option to show BLOCKED/IDLE indicators.
# Usage: tmux-claude-waiting.sh blocked|done|off
# Called from Claude Code hooks (Stop / Notification / UserPromptSubmit / SessionEnd).

# TMUX_PANE is set by tmux for processes running inside a pane.
target="${TMUX_PANE:-$(tmux display-message -p '#{pane_id}' 2>/dev/null)}"

if [ -z "$target" ]; then
    exit 0
fi

case "$1" in
    blocked)
        tmux set-option -t "$target" -p @claude-waiting blocked
        ;;
    done)
        tmux set-option -t "$target" -p @claude-waiting done
        ;;
    off)
        tmux set-option -t "$target" -p -u @claude-waiting
        ;;
esac
