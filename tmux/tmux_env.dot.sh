function ,tmux() {
    if [[ -z "$TMUX" ]]; then
        command tmux new-session \; \
            set pane-border-status top \; \
            set pane-border-format "#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fill=colour6],#[fill=colour15]},#[fill=colour15]},}#[align=left]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour6]>>>>>>>>>>,#[fg=colour15]━━━━━━━━━━},#[fg=colour15]━━━━━━━━━━},#{?#{==:#{@passthrough},on},#[dim]#[fg=colour8],#[fg=colour8]}━━━━━━━━━━}#[align=centre]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour0]#[bg=colour6],#[fg=colour0]#[bg=colour15]},#[fg=colour0]#[bg=colour15]},#{?#{==:#{@passthrough},on},#[nodim]#[fg=colour0]#[bg=colour8],#[fg=colour0]#[bg=colour8]}} #{?#{==:#{pane_current_command},tmux},tmux: ,}#($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path} #{pane_id} #{pane_current_command})#{?window_zoomed_flag, Z,}#{?#{==:#{@claude-waiting},blocked}, #[fg=colour5]#[reverse] ■ BLOCKED,#{?#{==:#{@claude-waiting},done}, #[noreverse]#{?pane_active, ,}#[fg=colour3]● IDLE,}} #[default]#[align=right]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour6]<<<<<<<<<<,#[fg=colour15]━━━━━━━━━━},#[fg=colour15]━━━━━━━━━━},#{?#{==:#{@passthrough},on},#[dim]#[fg=colour8],#[fg=colour8]}━━━━━━━━━━}"
        return
    fi
    if [[ -n "$MD_TMUX_OUTER" ]]; then
        echo "Already in a nested tmux session (outer=$MD_TMUX_OUTER)"
        return 1
    fi
    ,tmux--nested
}

function ,tmux-attach() {
    local show_all=0
    if [[ "$1" == "-a" ]]; then
        show_all=1
        shift
    fi

    if [[ -n "$MD_TMUX_OUTER" ]]; then
        echo "Already in a nested tmux session (outer=$MD_TMUX_OUTER)"
        return 1
    fi

    local in_tmux=0
    [[ -n "$TMUX" ]] && in_tmux=1

    local filter_prefix=""
    [[ $in_tmux -eq 1 ]] && filter_prefix="nested-"

    local sessions
    if [[ $show_all -eq 1 ]]; then
        sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null | \
            { if [[ -n "$filter_prefix" ]]; then grep "^${filter_prefix}"; else cat; fi })
    else
        sessions=$(tmux list-sessions -F '#{session_name}|#{session_attached}' 2>/dev/null | \
            grep '|0$' | cut -d'|' -f1 | \
            { if [[ -n "$filter_prefix" ]]; then grep "^${filter_prefix}"; else cat; fi })
    fi

    if [[ -z "$sessions" ]]; then
        echo "No matching sessions found"
        return 1
    fi

    local count selected
    count=$(echo "$sessions" | wc -l | tr -d ' ')

    if [[ $count -eq 1 ]]; then
        selected="$sessions"
    else
        selected=$(echo "$sessions" | fzf \
            --preview 's={}; tmux list-sessions 2>/dev/null | grep -F "$s:"; echo; tmux list-panes -t "$s" -s -F "  #{window_index}:#{pane_index} #{pane_title} [#{pane_current_command}] #{pane_current_path}" 2>/dev/null' \
            --preview-window=top:60% \
            --prompt="attach session> ")
    fi

    [[ -z "$selected" ]] && return 0

    if [[ $in_tmux -eq 1 ]]; then
        local outer_session
        outer_session=$(tmux display-message -p '#S')
        tmux set-option -p @nested on
        tmux set-option -p @inner_session "$selected"
        "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer_session"
        TMUX= tmux attach -t "$selected"

        # Inner tmux has exited/detached — clean up
        tmux set-option -p -u @inner_session
        tmux set-option -p -u @nested
        local state
        state=$(tmux show -t "$outer_session" -qv @passthrough 2>/dev/null)
        if [ "$state" = "on" ]; then
            "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer_session"
        fi
    else
        tmux attach -t "$selected"
    fi
}

# If this shell is running within tmux, add some extra utilities
if [[ -z $TMUX ]]; then return; fi

function ,tmux-reload() {
    # Reload tmux config and this file. Has to be done per pane.
    tmux source-file ~/.tmux.conf
    source $DOTFILES/tmux/tmux_env.dot.sh
    echo "Reloaded ~/.tmux.conf and $DOTFILES/tmux/tmux_env.sh"
}


# Show titles on panes, to help keep track when lots of TUI-like
# programs are running
function ,tmux-last-layout() {
    tmux select-layout -o
}

function ,tmux--nested() {
    local outer_session inner_name
    outer_session=$(tmux display-message -p '#S')
    inner_name="nested-${outer_session}-$(tmux display-message -p '#{pane_id}' | tr '%' '_')"
    tmux set-option -p @nested on
    tmux set-option -p @inner_session "$inner_name"
    "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer_session"
    TMUX= tmux new-session -s "$inner_name" "MD_TMUX_OUTER='$outer_session' bash" \; \
        set-environment MD_TMUX_OUTER "$outer_session" \; \
        set-option -w pane-border-lines double \; \
        set-option -w pane-border-status top \; \
        set-option -w pane-border-format "#[nodim]#[align=left]#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[fg=colour0]#[bg=colour15],#[fg=colour7]},#[fg=colour7]} #($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path})#{?window_zoomed_flag, Z,} #{?#{==:#{@claude-waiting},blocked},#[fg=colour5]#[reverse] ■ BLOCKED ,#{?#{==:#{@claude-waiting},done},#[noreverse]#[fg=colour3]● IDLE ,}}#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[bg=default]#[fg=colour15]════════════════════,},#{?#{==:#{@outer_passthrough},on},#[fg=colour7],#[dim]#[fg=colour7]}══════════}#[default]" \; \
        set-option -w pane-active-border-style "fg=colour7,dim" \; \
        set-option -w pane-border-style "fg=colour7,dim" \; \
        set-option status off \; \
        set-option status-position bottom \; \
        set-option status-justify left \; \
        set-option status-style "bg=colour0,fg=colour8" \; \
        set-option status-left "#[fg=colour8] #S " \; \
        set-option window-status-current-style "fg=colour6" \; \
        set-option window-status-style "fg=colour8" \; \
        set-option window-status-format " #I:#W#{?window_zoomed_flag,Z,} " \; \
        set-option window-status-current-format " #I:#W#{?window_zoomed_flag,Z,} " \; \
        set-hook after-new-window \
            "set-option status on ; \
             set-option -w pane-border-lines double ; \
             set-option -w pane-border-status top ; \
             set-option -w pane-border-format '#[nodim]#[align=left]#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[fg=colour0]#[bg=colour15],#[fg=colour7]},#[fg=colour7]} #($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path})#{?window_zoomed_flag, Z,} #{?#{==:#{@claude-waiting},blocked},#[fg=colour5]#[reverse] ■ BLOCKED ,#{?#{==:#{@claude-waiting},done},#[noreverse]#[fg=colour3]● IDLE ,}}#{?pane_active,#{?#{==:#{@outer_passthrough},on},#[bg=default]#[fg=colour15]════════════════════,},#{?#{==:#{@outer_passthrough},on},#[fg=colour7],#[dim]#[fg=colour7]}══════════}#[default]' ; \
             set-option -w pane-active-border-style 'fg=colour7,dim' ; \
             set-option -w pane-border-style 'fg=colour7,dim' ; \
             set-option window-status-format ' #I:#W#{?window_zoomed_flag,Z,} ' ; \
             set-option window-status-current-format ' #I:#W#{?window_zoomed_flag,Z,} ' ; \
             set-option window-status-current-style 'fg=colour6' ; \
             set-option window-status-style 'fg=colour8'" \; \
        set-hook window-unlinked \
            "if-shell '[ #{session_windows} -le 1 ]' 'set-option status off'"

    # Inner tmux has exited — clean up
    tmux set-option -p -u @inner_session
    tmux set-option -p -u @nested
    local state
    state=$(tmux show -t "$outer_session" -qv @passthrough 2>/dev/null)
    if [ "$state" = "on" ]; then
        "$DOTFILES/tmux/tmux-toggle-nested.sh" "$outer_session"
    fi
}

function ,tmux-toggle-nested() {
    $DOTFILES/tmux/tmux-toggle-nested.sh
}

function ,tmux-reset-nested() {
    tmux set -g prefix C-a
    tmux set -g @passthrough off
    tmux bind-key C-a send-prefix
    tmux set -u key-table
    tmux set -u status-style
    tmux set -g pane-active-border-style "fg=colour12"
    tmux set -g pane-border-style "fg=colour15"
    # Clear @nested from all panes
    local pane_id
    tmux list-panes -s -F '#{pane_id}' | while read pane_id; do
        tmux set-option -t "$pane_id" -p -u @nested 2>/dev/null
    done
    echo "Reset prefix to C-a, restored bindings and border styles"
}

function ,tmux-toggle-titles() {
    local current
    current=$(tmux show -v pane-border-status 2>/dev/null)
    if [[ "$current" == "top" ]]; then
        tmux set pane-border-status off
        echo "Pane borders off"
    else
        tmux set pane-border-status top
        tmux set pane-border-format "#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fill=colour6],#[fill=colour15]},#[fill=colour15]},}#[align=left]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour6]>>>>>>>>>>,#[fg=colour15]━━━━━━━━━━},#[fg=colour15]━━━━━━━━━━},#{?#{==:#{@passthrough},on},#[dim]#[fg=colour8],#[fg=colour8]}━━━━━━━━━━}#[align=centre]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour0]#[bg=colour6],#[fg=colour0]#[bg=colour15]},#[fg=colour0]#[bg=colour15]},#{?#{==:#{@passthrough},on},#[nodim]#[fg=colour0]#[bg=colour8],#[fg=colour0]#[bg=colour8]}} #{?#{==:#{pane_current_command},tmux},tmux: ,}#($DOTFILES/tmux/tmux-pane-path.sh #{pane_current_path} #{pane_id} #{pane_current_command})#{?window_zoomed_flag, Z,}#{?#{==:#{@claude-waiting},blocked}, #[fg=colour5]#[reverse] ■ BLOCKED,#{?#{==:#{@claude-waiting},done}, #[noreverse]#{?pane_active, ,}#[fg=colour3]● IDLE,}} #[default]#[align=right]#{?pane_active,#{?#{==:#{pane_current_command},tmux},#{?#{==:#{@passthrough},on},#[fg=colour6]<<<<<<<<<<,#[fg=colour15]━━━━━━━━━━},#[fg=colour15]━━━━━━━━━━},#{?#{==:#{@passthrough},on},#[dim]#[fg=colour8],#[fg=colour8]}━━━━━━━━━━}"
        echo "Pane borders on"
    fi
}
