# Create neither *.pyc files nor __pycache__ directories.
export PYTHONDONTWRITEBYTECODE=1

# New breakpoint() handling
export PYTHONBREAKPOINT=pudb.start

which pip >/dev/null && eval "$(pip completion --bash)"

# By default, Python's virtualenv will modify the prompt when a virtualenv is
# active. Disable this.
export VIRTUAL_ENV_DISABLE_PROMPT=1

if [ ! -z "$(which python3)" ]; then export VIRTUALENVWRAPPER_PYTHON=$(which python3); fi
if [ ! -z "$(which virtualenv)" ]; then export VIRTUALENVWRAPPER_VIRTUALENV=$(which virtualenv); fi

export VIRTUALENVWRAPPER_HOOK_DIR="$DOTFILES/virtualenvwrapper_hooks"
export WORKON_HOME=$HOME/.virtualenvs
mkdir -p "$WORKON_HOME"

VENV_WRAPPER_SCRIPT=$(which virtualenvwrapper.sh)
if [ ! -z "$VENV_WRAPPER_SCRIPT" ]; then source "$VENV_WRAPPER_SCRIPT"; fi

# Virtualenv workflow:
# - Use pyenv to install python versions, but don't use the provided shims.
# - Use mkvirtualenv to make venvs with a specific pyenv version.
if [ $(command -v pyenv) ]; then
    alias ,pymk2='mkvirtualenv -p $(pyenv which python2)'
    alias ,pymk3='mkvirtualenv -p $(pyenv which python3)'
    alias ,pymktmp2='mktmpenv -p $(pyenv which python2)'
    alias ,pymktmp3='mktmpenv -p $(pyenv which python3)'
fi

# [2024-02-12] pipx on macos uses this path
,path /Users/matt/.local/bin

function ,pypath {
    # usage: ,pypath [--prepend] [<directories>]
    #
    # Adds current directory or given directories to path. If --prepend not
    # given, directories are appended.
    #
    # Also updates $_OLD_VIRTUAL_PATH set by Python's virtualenv, so that path
    # changes persist through a venv deactivate command.
    if [[ -z "$1" ]]; then
        echo 'PYTHONPATH: '
        IFS=: eval printf "%s\\\n" \$${1:-PYTHONPATH}
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

        if [[ ":$PYTHONPATH:" != *":$fullpath:"* ]]; then

            if [[ "$prepend" = true ]]; then
                export PYTHONPATH="$fullpath:$PYTHONPATH"
            else
                export PYTHONPATH="$PYTHONPATH:$fullpath"
            fi
        fi
    done
   ,pypath
}

function ,pycrm() {
    find "$@" -type f -name '*.pyc' -print0 | xargs -0 rm
    find "$@" -type d -iname '__pycache__' -print0 | xargs -0 rm -r
}

function ,pydebug() {
    # Prepare terminal to be a pudb debugger. See https://documen.tician.de/pudb/starting.html
    echo "Set PUDB_TTY:"
    echo "export PUDB_TTY=$(tty)" 
    perl -MPOSIX -e pause
}
