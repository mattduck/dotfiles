#!/bin/sh
# Show a short pane path: repo-relative if in a git repo, otherwise ~/... or absolute.
# Usage: tmux-pane-path.sh <path> [pane_id] [pane_current_command]
# When pane_current_command is "tmux", query the inner session's active pane path instead.

path="$1"
pane_id="$2"
pane_cmd="$3"

# If this pane is running a nested tmux, get the inner session's active pane path
if [ "$pane_cmd" = "tmux" ] && [ -n "$pane_id" ]; then
    inner_session=$(tmux display-message -t "$pane_id" -p '#{@inner_session}' 2>/dev/null)
    if [ -n "$inner_session" ]; then
        inner_path=$(tmux display-message -t "$inner_session" -p '#{pane_current_path}' 2>/dev/null)
        if [ -n "$inner_path" ]; then
            path="$inner_path"
        fi
    fi
fi

# Use resolved path for git comparison but original path for non-git display.
resolved=$(cd "$path" 2>/dev/null && pwd -P) || resolved="$path"
toplevel=$(git -C "$resolved" rev-parse --show-toplevel 2>/dev/null) || {
    echo "$path" | sed "s|^$HOME|~|"
    exit
}
repo=$(basename "$toplevel")
rel=${resolved#"$toplevel"}
echo "${repo}${rel}"
