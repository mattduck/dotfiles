#!/usr/bin/env bash
if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# [2023-05-28] Don't print 'The default interactive shell is now zsh' - see https://support.apple.com/en-us/HT208050
export BASH_SILENCE_DEPRECATION_WARNING=1

,path --prepend "/opt/homebrew/bin"

# Use GNU coreutils - it's easier if programs have the same flags between
# machines, same man pages etc.
,path --prepend "$(brew --prefix)/opt/coreutils/libexec/gnubin"
export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"

# golang
,path "$(brew --prefix)opt/go/libexec/bin/"

# Latex
if [ -d /usr/local/texlive/2023 ]; then
  ,path /usr/local/texlive/2023/bin/universal-darwin/
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
