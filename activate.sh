# activate.sh
# Sets up the shell environment.

# Get absolute path of this directory. Below snippet from
# stackoverflow.com/questions/59895:
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
  THIS_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$THIS_DIR/$SOURCE" 
done

export DOTFILES="$( cd -P "$( dirname "$SOURCE" )" && pwd )"


function ,path-add() {
    # usage: ,path-add [--prepend] [<directories>]
    #
    # Adds current directory or given directories to path. If --prepend not
    # given, directories are appended.
    #
    # Also updates $_OLD_VIRTUAL_PATH set by Python's virtualenv, so that path
    # changes persist through a venv deactivate command.

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

    if [[ $(uname -a) == *Darwin* ]]; then
        find_re="find -E $DOTFILES -regex"
    else
        find_re="find $DOTFILES -regextype posix-extended -regex"
    fi

    for f in $($find_re ".+\.dot\.[0-9][0-9]\.sh" | sort -t "." -k 2); do
        echo $f
    done

    for f in $(find $DOTFILES -name "*.dot.sh"); do
        echo "$f"
    done
}


,path-add $(find $DOTFILES -type d -name bin)

for f in $(,dotfiles-ls); do
    source "$f"
done
