# Create neither *.pyc files nor __pycache__ directories.
export PYTHONDONTWRITEBYTECODE=1

# Automatic fab completion
# shellcheck source=/dev/null
. "$DOTFILES/bash/fabric-completion/fabric-completion.bash"
export FAB_COMPLETION_CACHE_TASKS=false

eval "$(pip completion --bash)"

# By default, Python's virtualenv will modify the prompt when a virtualenv is
# active. Disable this.
export VIRTUAL_ENV_DISABLE_PROMPT=1

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
export VIRTUALENVWRAPPER_HOOK_DIR="$DOTFILES/virtualenvwrapper_hooks"
export WORKON_HOME=$HOME/.virtualenvs
mkdir -p "$WORKON_HOME"
source $(which virtualenvwrapper.sh)

# Virtualenv workflow:
# - Use pyenv to install python versions, but don't use the provided shims.
# - Use mkvirtualenv to make venvs with a specific pyenv version.

alias ,pyworkon="workon"
alias ,pydeactivate="deactivate"
alias ,pywipe="wipeenv"
alias ,pycdsite="cdsitepackages"
alias ,pycd="cdvirtualenv"
alias ,pymk2="mkvirtualenv -p $(pyenv which python2)"
alias ,pymk3="mkvirtualenv -p $(pyenv which python3)"
alias ,pymktmp2="mktmpenv -p $(pyenv which python2)"
alias ,pymktmp3="mktmpenv -p $(pyenv which python3)"
alias ,pywhich="python --version; which python; which pip"
alias ,pyrm="rmvirtualen"
alias ,pylssite="lssitepackages"


function ,pypathadd() {
    # usage: ,pypathadd [--prepend] [<directories>]
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

        if [[ ":$PYTHONPATH:" != *":$fullpath:"* ]]; then

            if [[ "$prepend" = true ]]; then
                export PYTHONPATH="$fullpath:$PYTHONPATH"
            else
                export PYTHONPATH="$PYTHONPATH:$fullpath"
            fi
        fi
    done
   ,pypathecho
}
