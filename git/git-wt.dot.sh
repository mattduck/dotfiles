# ,gwt - git worktree convenience commands
#
# Usage:
#   ,gwt              - fzf picker to jump to a worktree
#   ,gwt <ref>        - go to worktree (auto-creates if needed)
#   ,gwt add          - create next numbered worktree
#   ,gwt rm [<ref>]   - remove worktree (current if no ref given)
#   ,gwt ls           - list all worktrees
#
# <ref> can be a number (2, 3), ordinal (second, third), or name.
# Branches are named wt/<ordinal> (numbered) or wt/<name> (named).
# Worktree directories live in ~/f/worktrees/<repo>.<N|name>.

_GWT_WORKTREE_DIR="$HOME/f/worktrees"
_GWT_ORDINALS=(
    _ first second third fourth fifth sixth seventh eighth ninth tenth
    eleventh twelfth thirteenth fourteenth fifteenth sixteenth
    seventeenth eighteenth nineteenth twentieth
)

_gwt_ordinal_to_num() {
    local arg="$1"
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
        echo "$arg"
        return 0
    fi
    for i in "${!_GWT_ORDINALS[@]}"; do
        if [[ "${_GWT_ORDINALS[$i]}" == "$arg" ]]; then
            echo "$i"
            return 0
        fi
    done
    return 1
}

_gwt_num_to_branch() {
    local n="$1"
    if (( n < 2 || n > ${#_GWT_ORDINALS[@]} - 1 )); then
        echo "Number out of range (2-${#_GWT_ORDINALS[@]}): $n" >&2
        return 1
    fi
    echo "wt/${_GWT_ORDINALS[$n]}"
}

_gwt_main_worktree() {
    git worktree list --porcelain | head -1 | sed 's/^worktree //'
}

_gwt_repo_name() {
    local base
    base="$(basename "$1")"
    # Strip YYYY-MM-DD- or YYYY-MM- prefix
    echo "$base" | sed -E 's/^[0-9]{4}-[0-9]{2}(-[0-9]{2})?-//'
}

_gwt_find_by_branch() {
    local branch="$1"
    git worktree list --porcelain | awk -v branch="$branch" '
        /^worktree / { path = substr($0, 10) }
        /^branch refs\/heads\// {
            b = substr($0, 21)
            if (b == branch) print path
        }
    '
}

# Parse worktree list into tab-separated lines:
#   path \t branch \t commit_hash
_gwt_parse_worktrees() {
    git worktree list --porcelain | awk '
        /^worktree / { path = substr($0, 10) }
        /^HEAD /     { hash = substr($0, 6) }
        /^branch /   { branch = substr($0, 8); sub("refs/heads/", "", branch) }
        /^detached/  { branch = "(detached)" }
        /^$/ {
            if (length(path) > 0) printf("%s\t%s\t%s\n", path, branch, hash)
            path = ""; branch = ""; hash = ""
        }
        END {
            if (length(path) > 0) printf("%s\t%s\t%s\n", path, branch, hash)
        }
    '
}

# Format a worktree line for display
# Args: path, branch, hash, main_path
_gwt_format_line() {
    local path="$1" branch="$2" hash="$3" main_path="$4"
    local dirty commit_date commit_msg short_hash age dir_name

    # Dirty indicator
    if [[ -n "$(git -C "$path" status --porcelain 2>/dev/null)" ]]; then
        dirty="*"
    else
        dirty=" "
    fi

    # Directory basename
    dir_name="$(basename "$path")"

    # Last commit date (absolute + relative) and message
    short_hash="${hash:0:7}"
    commit_date="$(git -C "$path" log -1 --format='%as (%ar)' 2>/dev/null)"
    commit_msg="$(git -C "$path" log -1 --format='%s' 2>/dev/null | cut -c1-50)"

    # Worktree creation date
    if [[ "$path" == "$main_path" ]]; then
        age="-"
    else
        age="$(/usr/bin/stat -f '%SB' -t '%Y-%m-%d' "$path" 2>/dev/null || echo '?')"
    fi

    # Output: path<TAB>display
    # Columns: branch (directory)  commit_date  commit  wt:created
    local branch_dir="$branch ($dir_name)"
    printf '%s\t%s %-*s  %-28s  %s %-50s  wt:%s\n' \
        "$path" "$dirty" "${_GWT_COL_WIDTH:-36}" "$branch_dir" "$commit_date" "$short_hash" "$commit_msg" "$age"
}

function ,gwt() {
    local main_path repo_name
    main_path="$(_gwt_main_worktree)"
    repo_name="$(_gwt_repo_name "$main_path")"

    # Create next numbered worktree and cd into it
    _gwt_add() {
        local num branch dir
        num=2
        while [[ -d "$_GWT_WORKTREE_DIR/$repo_name.$num" ]]; do
            ((num++))
        done

        branch="$(_gwt_num_to_branch "$num")" || return 1
        dir="$_GWT_WORKTREE_DIR/$repo_name.$num"

        mkdir -p "$_GWT_WORKTREE_DIR"
        if git show-ref --verify --quiet "refs/heads/$branch"; then
            git worktree add "$dir" "$branch" || return 1
        else
            git worktree add -b "$branch" "$dir" || return 1
        fi

        builtin cd "$dir"
    }

    # Go to worktree by name/number/ordinal, auto-creating if needed
    _gwt_go() {
        local arg="$1"

        # 1 / first → main worktree
        if [[ "$arg" == "1" || "$arg" == "first" ]]; then
            builtin cd "$main_path"
            return 0
        fi

        # Try as number or ordinal
        local num
        if num="$(_gwt_ordinal_to_num "$arg")"; then
            local dir="$_GWT_WORKTREE_DIR/$repo_name.$num"
            if [[ -d "$dir" ]]; then
                builtin cd "$dir"
                return 0
            fi

            # Check if the branch is already checked out elsewhere
            local branch
            branch="$(_gwt_num_to_branch "$num")" || return 1
            local existing
            existing="$(_gwt_find_by_branch "$branch")"
            if [[ -n "$existing" ]]; then
                builtin cd "$existing"
                return 0
            fi

            # Auto-create numbered worktree
            mkdir -p "$_GWT_WORKTREE_DIR"
            if git show-ref --verify --quiet "refs/heads/$branch"; then
                git worktree add "$dir" "$branch" || return 1
            else
                git worktree add -b "$branch" "$dir" || return 1
            fi
            builtin cd "$dir"
            return 0
        fi

        # Try as named worktree directory
        local named_dir="$_GWT_WORKTREE_DIR/$repo_name.$arg"
        if [[ -d "$named_dir" ]]; then
            builtin cd "$named_dir"
            return 0
        fi

        # Try as branch name (wt/<arg>)
        local wt_path
        wt_path="$(_gwt_find_by_branch "wt/$arg")"
        if [[ -n "$wt_path" ]]; then
            builtin cd "$wt_path"
            return 0
        fi

        # Auto-create named worktree
        local branch="wt/$arg"
        mkdir -p "$_GWT_WORKTREE_DIR"
        if git show-ref --verify --quiet "refs/heads/$branch"; then
            git worktree add "$named_dir" "$branch" || return 1
        else
            git worktree add -b "$branch" "$named_dir" || return 1
        fi
        builtin cd "$named_dir"
        return 0
    }

    _gwt_rm() {
        local arg="${1:-}"
        local target_dir

        if [[ -z "$arg" ]]; then
            target_dir="$(pwd -P)"
            if [[ "$target_dir" == "$main_path" ]]; then
                echo "Cannot remove the main worktree" >&2
                return 1
            fi
            local in_worktree=false
            while IFS= read -r line; do
                if [[ "$target_dir" == "$line" || "$target_dir" == "$line"/* ]]; then
                    in_worktree=true
                    target_dir="$line"
                    break
                fi
            done < <(git worktree list --porcelain | awk '/^worktree / { print substr($0, 10) }')
            if ! "$in_worktree"; then
                echo "Not in a worktree" >&2
                return 1
            fi
        else
            if [[ "$arg" == "1" || "$arg" == "first" ]]; then
                echo "Cannot remove the main worktree" >&2
                return 1
            fi
            local num
            if num="$(_gwt_ordinal_to_num "$arg")"; then
                target_dir="$_GWT_WORKTREE_DIR/$repo_name.$num"
            else
                # Try as named worktree directory
                target_dir="$_GWT_WORKTREE_DIR/$repo_name.$arg"
                if [[ ! -d "$target_dir" ]]; then
                    # Try finding by branch name
                    target_dir="$(_gwt_find_by_branch "wt/$arg")"
                    if [[ -z "$target_dir" ]]; then
                        echo "No worktree found for: $arg" >&2
                        return 1
                    fi
                fi
            fi
        fi

        if [[ ! -d "$target_dir" ]]; then
            echo "Worktree directory does not exist: $target_dir" >&2
            return 1
        fi

        # Check for dirty state
        if [[ -n "$(git -C "$target_dir" status --porcelain)" ]]; then
            echo "Worktree is dirty (uncommitted changes or untracked files):" >&2
            git -C "$target_dir" status --short >&2
            return 1
        fi

        # cd to main if we're inside the worktree being removed
        local cwd
        cwd="$(pwd -P)"
        if [[ "$cwd" == "$target_dir" || "$cwd" == "$target_dir"/* ]]; then
            builtin cd "$main_path"
        fi

        git worktree remove "$target_dir"
    }

    # Compute max branch (dir) column width from parsed worktree data
    _gwt_calc_col_width() {
        local max=0
        while IFS=$'\t' read -r path branch _hash; do
            local dir_name w
            dir_name="$(basename "$path")"
            w=$(( ${#branch} + 3 + ${#dir_name} ))
            (( w > max )) && max=$w
        done
        echo "$max"
    }

    _gwt_ls() {
        local wt_data
        wt_data="$(_gwt_parse_worktrees)"
        _GWT_COL_WIDTH="$(echo "$wt_data" | _gwt_calc_col_width)"
        while IFS=$'\t' read -r path branch hash; do
            _gwt_format_line "$path" "$branch" "$hash" "$main_path" | cut -f2-
        done <<< "$wt_data"
    }

    _gwt_fzf() {
        local wt_data
        wt_data="$(_gwt_parse_worktrees)"
        _GWT_COL_WIDTH="$(echo "$wt_data" | _gwt_calc_col_width)"
        local lines=()
        while IFS=$'\t' read -r path branch hash; do
            [[ -z "$path" ]] && continue
            local formatted
            formatted="$(_gwt_format_line "$path" "$branch" "$hash" "$main_path")"
            lines+=("$formatted")
        done <<< "$wt_data"

        if [[ ${#lines[@]} -eq 0 ]]; then
            echo "No worktrees found" >&2
            return 1
        fi

        local selected
        selected="$(printf '%s\n' "${lines[@]}" | \
            fzf --delimiter=$'\t' \
                --with-nth=2 \
                --preview='git -C {1} show --stat --color=always' \
                --preview-window=up:75% \
                --no-sort \
                --ansi \
        )" || return 0

        local target
        target="$(echo "$selected" | cut -f1)"
        if [[ -n "$target" ]]; then
            builtin cd "$target"
        fi
    }

    case "${1:-}" in
        add)  _gwt_add ;;
        rm)   _gwt_rm "${2:-}" ;;
        ls|list) _gwt_ls ;;
        "")   _gwt_fzf ;;
        *)    _gwt_go "$1" ;;
    esac
}
