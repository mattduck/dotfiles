# ,gwt - git worktree convenience commands
#
# Usage:
#   ,gwt add          - create next numbered worktree and cd into it
#   ,gwt cd <ref>     - cd to worktree (auto-creates if needed)
#   ,gwt rm [<ref>]   - remove worktree (current if no ref given)
#   ,gwt ls           - list all worktrees
#
# <ref> can be a number (2, 3), ordinal (second, third), or branch name.
# Branches are named wt/second, wt/third, etc.
# Worktree directories live in ~/f/worktrees/<repo>.<N>.

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

function ,gwt() {
    local main_path repo_name
    main_path="$(_gwt_main_worktree)"
    repo_name="$(_gwt_repo_name "$main_path")"

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

    _gwt_cd() {
        local arg="${1:-}"
        if [[ -z "$arg" ]]; then
            echo "Usage: ,gwt cd <number|ordinal|branch>" >&2
            return 1
        fi

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

            # Auto-create
            mkdir -p "$_GWT_WORKTREE_DIR"
            if git show-ref --verify --quiet "refs/heads/$branch"; then
                git worktree add "$dir" "$branch" || return 1
            else
                git worktree add -b "$branch" "$dir" || return 1
            fi
            builtin cd "$dir"
            return 0
        fi

        # Try as branch name
        local wt_path
        wt_path="$(_gwt_find_by_branch "wt/$arg")"
        if [[ -n "$wt_path" ]]; then
            builtin cd "$wt_path"
            return 0
        fi

        echo "No worktree found for: $arg" >&2
        return 1
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
                target_dir="$(_gwt_find_by_branch "wt/$arg")"
                if [[ -z "$target_dir" ]]; then
                    echo "No worktree found for: $arg" >&2
                    return 1
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

    case "${1:-}" in
        add)  _gwt_add ;;
        cd)   _gwt_cd "${2:-}" ;;
        rm)   _gwt_rm "${2:-}" ;;
        ls|list) git worktree list ;;
        [0-9]*) _gwt_cd "$1" ;;
        "")   git worktree list ;;
        *)
            echo "Usage: ,gwt [add|cd|rm|ls|N]" >&2
            echo "" >&2
            echo "  (no args)    List all worktrees" >&2
            echo "  add          Create next numbered worktree" >&2
            echo "  cd <ref>     Go to worktree (auto-creates if needed)" >&2
            echo "  N            Shorthand for cd N" >&2
            echo "  rm [<ref>]   Remove worktree (current if no ref)" >&2
            echo "  ls           List all worktrees" >&2
            echo "" >&2
            echo "<ref>: number (2,3), ordinal (second,third), or branch name" >&2
            return 1
            ;;
    esac
}
