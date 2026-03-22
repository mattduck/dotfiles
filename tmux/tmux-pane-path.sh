#!/bin/sh
# Show a short pane path: repo-relative if in a git repo, otherwise ~/... or absolute.
# Use resolved path for git comparison but original path for non-git display.
resolved=$(cd "$1" 2>/dev/null && pwd -P) || resolved="$1"
toplevel=$(git -C "$resolved" rev-parse --show-toplevel 2>/dev/null) || {
    echo "$1" | sed "s|^$HOME|~|"
    exit
}
repo=$(basename "$toplevel")
rel=${resolved#"$toplevel"}
echo "${repo}${rel}"
