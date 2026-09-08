#!/usr/bin/env bash
# Toggle between collapsing single-pane windows into the current window
# and expanding panes back out into separate windows.

session=$(tmux display-message -p '#{session_name}')
collapsed=$(tmux show -t "$session" -qv @collapsed 2>/dev/null)

if [ "$collapsed" = "on" ]; then
    # Expand: only break out panes that were merged in, not pre-existing ones
    merged=$(tmux show -t "$session" -qv @collapsed_panes 2>/dev/null)
    IFS=',' read -ra merged_panes <<< "$merged"
    for pid in "${merged_panes[@]}"; do
        [ -z "$pid" ] && continue
        tmux break-pane -d -s "$pid" 2>/dev/null
    done
    tmux set -t "$session" -u @collapsed_panes

    # Re-apply inner window styles. These are duplicated from the
    # ,tmux--nested function in tmux_env.dot.sh (initial set-option
    # calls and the after-new-window hook). Keep in sync.
    tmux list-windows -t "$session" -F '#{window_index}' | while read -r widx; do
        tmux set-option -w -t "${session}:${widx}" pane-border-lines double
        tmux set-option -w -t "${session}:${widx}" pane-border-status top
        tmux set-option -w -t "${session}:${widx}" pane-border-format '#[nodim]#[align=left]#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[fg=colour0]#[bg=colour15],#[fg=colour7]},#[fg=colour7]} #{?pane_active,#{?#{==:#{@outer_passthrough},on},> ,},}#($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path})#{?window_zoomed_flag, Z,}#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[bg=default]#[fg=colour15]════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════,},#{?#{==:#{@outer_passthrough},on},#[fg=colour7],#[dim]#[fg=colour7]}══════════}#[default]'
        tmux set-option -w -t "${session}:${widx}" pane-active-border-style 'fg=colour7,dim'
        tmux set-option -w -t "${session}:${widx}" pane-border-style 'fg=colour7,dim'
    done
    tmux set-option -t "$session" status on
    tmux set-option -t "$session" window-status-format ' #I '
    tmux set-option -t "$session" window-status-current-format ' #I*'
    tmux set-option -t "$session" window-status-current-style 'fg=colour6,bold'
    tmux set-option -t "$session" window-status-style 'fg=colour8'

    tmux set -t "$session" @collapsed off
else
    # Collapse: pull in panes from single-pane windows.
    # Collect pane IDs upfront since join-pane mutates the window list.
    current=$(tmux display-message -p '#{window_index}')
    panes_to_merge=()
    while read -r idx count; do
        [ "$idx" = "$current" ] && continue
        [ "$count" -eq 1 ] || continue
        panes_to_merge+=($(tmux list-panes -t ":${idx}" -F '#{pane_id}'))
    done < <(tmux list-windows -F '#{window_index} #{window_panes}')
    for pid in "${panes_to_merge[@]}"; do
        tmux join-pane -d -s "$pid" -t ":${current}"
    done
    pane_count=$(tmux list-panes -F '#{pane_id}' | wc -l | tr -d ' ')
    if [ "$pane_count" -eq 2 ]; then
        width=$(tmux display-message -p '#{window_width}')
        if [ "$width" -gt 180 ]; then
            tmux select-layout even-horizontal
        else
            tmux select-layout even-vertical
        fi
    else
        tmux select-layout tiled
    fi
    # Remember which panes were merged so expand only breaks those out
    tmux set -t "$session" @collapsed_panes "$(IFS=,; echo "${panes_to_merge[*]}")"
    tmux set -t "$session" @collapsed on
fi
