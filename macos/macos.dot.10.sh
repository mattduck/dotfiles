#!/usr/bin/env bash
if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# [2023-05-28] Don't print 'The default interactive shell is now zsh' - see https://support.apple.com/en-us/HT208050
export BASH_SILENCE_DEPRECATION_WARNING=1

,path --prepend "/opt/homebrew/bin"

# Use Brew instead of system Ruby
,path --prepend "/usr/local/opt/ruby/bin"

# Add TeX to path (for org-mode PDF exporting)
,path --prepend "/usr/local/texlive/2014/bin/universal-darwin"
,path --prepend "/Library/TeX/Distributions/.DefaultTeX/Contents/Programs/texbin/"

# Use GNU coreutils - it's easier if programs have the same flags between
# machines, same man pages etc.
,path --prepend "$(brew --prefix)/opt/coreutils/libexec/gnubin"
export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"

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
export MITSCHEME_LIBRARY_PATH="/usr/local/Cellar/mit-scheme/9.2_1/lib/mit-scheme-c/"


# Emacs ------------------
alias emacs-app="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacs-app-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
alias emacs-app-client="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t"

# New emacs in terminal
alias en="emacs-app -nw"

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


# Golang -------------
,path "$(brew --prefix)opt/go/libexec/bin/"

# iTerm 2 - toggle colour profiles
function ,ts () {
    if [ ! "$(type -t it2check)" ]; then echo "it2check not found" && return 1; fi
    # if [ ! "$(it2check)" ]; then echo "Not in iTerm" && return 1; fi
    if [ ! "$(type -t it2setcolor)" ]; then echo "it2setcolor not found" && return 1; fi

    tspath=/tmp/.it2solarized
    if [ ! -f "$tspath" ]; then echo 1 > "$tspath"; fi
    val=`cat "$tspath"`
    if [ "$val" -eq 0 ]; then
        it2setcolor preset 'Solarized Light' || return 1
        echo 1 > "$tspath"
    else
        it2setcolor preset 'Solarized Dark' || return 1
        echo 0 > "$tspath"
    fi
}
