#!/bin/bash
#
# activate.sh
#
# - Sets up the shell environment.
#
# - Dependency functions are defined here for simplicity, rather than in
#   separate scripts or ".dot.sh" files.


function ,realpath() {
    # Get absolute path without depending on `realpath` or `readlink`, which
    # aren't installed on OS X by default (at least on some older versions).
    echo "$(cd "$(dirname "$1")" && pwd -P)/$(basename "$1")"
}


function ,path() {
    # usage: ,path [--prepend] [<directories>]
    #
    # Adds current directory or given directories to path. If --prepend not
    # given, directories are appended.
    #
    # Also updates $_OLD_VIRTUAL_PATH set by Python's virtualenv, so that path
    # changes persist through a venv deactivate command.
    if [[ -z "$1" ]]; then
        echo 'PATH: '
        IFS=: eval printf "%s\\\n" \$${1:-PATH}
        return 0
    fi

    if [[ $1 = "--prepend" ]]; then
        prepend=true
        paths=(${@:2})
    else
        prepend=false
        paths=($@)
    fi

    if [ -z "$paths" ]; then
        paths[0]=$PWD
    fi

   for path in "${paths[@]}"; do

       # Ignore paths that don't exist
       if [ ! -f "$path" ] && [ ! -d "$path" ]; then
           continue
        fi

        fullpath="$(cd "$path"; echo $PWD)"

        if [[ ":$PATH:" != *":$fullpath:"* ]]; then

            if [[ "$prepend" = true ]]; then
                export PATH="$fullpath:$PATH"
                export _OLD_VIRTUAL_PATH="$fullpath:$PATH"
            else
                export PATH="$PATH:$fullpath"
                export _OLD_VIRTUAL_PATH="$PATH:$fullpath"
            fi
        fi
    done
}


function ,dotfiles-ls {
    # In numerical order, list paths ending in .dot.xx.sh, then list paths
    # ending in .dot.sh.
    local DOT_NUM_REGEX='.*/.*\.dot\.[0-9][0-9]\.sh'
    local DOT_SH_PATTERN='*.dot.sh'
    local excludes=(
        vim
        virtualenvwrapper_hooks
        ipython.symlink
        hammerspoon.symlink
        emacs.d.symlink
        emacs.2023.symlink
        config.symlink
    )

    # Build -path exclude expressions
    local prune_expr=""
    for dir in "${excludes[@]}"; do
        prune_expr+="-path \"$DOTFILES/$dir\" -o "
    done
    prune_expr=${prune_expr::-4}  # remove trailing " -o "

    # Function to safely run find with pruning and filtering
    if [[ $(uname -s) == "Darwin" ]]; then
        # macOS
        dot_num_files=$(eval "find -E \"$DOTFILES\" \\( $prune_expr \\) -prune -o -regex '$DOT_NUM_REGEX' -print | sort -t '.' -k 2")
        dot_sh_files=$(eval "find -E \"$DOTFILES\" \\( $prune_expr \\) -prune -o -name '$DOT_SH_PATTERN' -print")
    else
        # Linux
        dot_num_files=$(eval "find \"$DOTFILES\" -regextype posix-extended \\( $prune_expr \\) -prune -o -regex '$DOT_NUM_REGEX' -print | sort -t '.' -k 2")
        dot_sh_files=$(eval "find \"$DOTFILES\" -regextype posix-extended \\( $prune_expr \\) -prune -o -name '$DOT_SH_PATTERN' -print")
    fi

    echo "$dot_num_files"
    echo "$dot_sh_files"

}


# $DOTFILES represents this directory
THIS_FILE=${BASH_SOURCE[0]}
export DOTFILES=$(dirname "$(,realpath "$THIS_FILE")")

# Append all "bin" directories in $DOTFILES to $PATH
#
# Append bin directories. Used to find these automatically but doing it manually
# for perf + to ensure we only include stuff I actually want
,path macos/bin
,path bin
,path fzf-tab-completion/readline/bin

# Source all ".dot.sh" files in $DOTFILES
for f in $(,dotfiles-ls); do
    source "$f"
done
