#!/bin/sh
# Print a brief directory name for tmux window titles.
# Uses git repo root name if in a repo, otherwise the directory basename.
# Home directory is shown as "~".
dir="$1"
if [ "$dir" = "$HOME" ]; then
    echo "~"
    exit 0
fi
root=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null)
if [ -n "$root" ]; then
    basename "$root"
else
    basename "$dir"
fi
