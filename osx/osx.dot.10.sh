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

# MIT-Scheme
export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

alias emacs-app="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacs-app-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
alias emacs-app-client="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
alias et="emacs-app-client -t"  # emacs client in terminal
alias egui="emacs-app-client -n"  # emacs client in gui
alias en="emacs-app -nw"  # new emacs in terminal

alias vi="vim" # Otherwise vi will point to /usr/bin, and vim to the brew dir.

function e () {
    if [ $# -eq 0 ]; then
        emacs-app-client -t .
    else
        emacs-app-client -t $@
    fi
}

function eview() {
    if [ $# -eq 0 ]; then
        emacs-app -nw "." --eval '(setq buffer-read-only t)'
    else
        emacs-app -nw "$1" --eval '(setq buffer-read-only t)'
    fi
}

function ,finder-pull() {
    # cd to topmost Finder window directory
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}

# Try using this new iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
