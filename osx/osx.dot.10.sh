#!/usr/bin/env bash
if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# Use Brew instead of system Ruby
,path-add --prepend "/usr/local/opt/ruby/bin"

# Add TeX to path (for org-mode PDF exporting)
,path-add --prepend "/usr/local/texlive/2014/bin/universal-darwin"

# Use GNU coreutils - it's easier if programs have the same flags between
# machines, same man pages etc.
,path-add --prepend "/usr/local/opt/coreutils/libexec/gnubin"
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# git __ps1 and completion aren't available by default on my Yosemite machine -
# either due to new git version or some OS change.
git_prompt_path=~/bin/git-prompt.sh
if [ -f "$git_prompt_path" ]; then
    source "$git_prompt_path"
fi
git_completion_path=~/bin/git-completion.bash
if [ -f "$git_completion_path" ]; then
    source "$git_completion_path"
fi

# Bash completion lives in the brew directory
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

function ,finder-pull() {
    # cd to topmost Finder window directory
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}

# Try using this new iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


# MIT-Scheme -------------
export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"


# Emacs ------------------
alias emacs-app="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacs-app-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
alias emacs-app-client="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

# New emacs in terminal
alias en="emacs-app -nw"

# New emacs in terminal, read-only
function eview() {
    if [ $# -eq 0 ]; then
        emacs-app -nw "." --eval '(setq buffer-read-only t)'
    else
        emacs-app -nw "$1" --eval '(setq buffer-read-only t)'
    fi
}

# Emacs gui client.
function egui () {
    if [ $# -eq 0 ]; then
        args='.'
    else
        args=$@
    fi
    # Create a new elscreen tab before visiting the file. I prefer this because
    # it ensures that opening a file from the terminal doesn't interfere with
    # existing windows.
    emacs-app-client -n --eval "(elscreen-goto (elscreen-create))" &>/dev/null
    emacs-app-client -n $args
}

# Emacs terminal client.
function e () {
    if [ $# -eq 0 ]; then
        emacs-app-client -t .
    else
        emacs-app-client -t $@
    fi
}


# vim -------------
alias vi="vim" # Otherwise vi will point to /usr/bin, and vim to the brew dir.


# Virtualenvwrapper -------------
# NOTE - I won't use PROJECT_HOME for now.
# NOTE - for this to work I might need to run /usr/local/opt/python/bin/pip
# install virtualenv virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=$(brew --prefix)/bin/python
export VIRTUALENVWRAPPER_VIRTUALENV=$(brew --prefix)/bin/virtualenv
export VIRTUALENVWRAPPER_HOOK_DIR="$DOTFILES/virtualenvwrapper_hooks"
export WORKON_HOME=$HOME/.virtualenvs

mkdir -p "$WORKON_HOME"
source $(which virtualenvwrapper.sh)


# Golang -------------
,path-add "/usr/local/opt/go/libexec/bin/"
