#!/usr/bin/env bash
if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# [2023-05-28] Don't print 'The default interactive shell is now zsh' - see https://support.apple.com/en-us/HT208050
export BASH_SILENCE_DEPRECATION_WARNING=1

,path --prepend "/opt/homebrew/bin"

brew_prefix=/opt/homebrew

# Use GNU coreutils - it's easier if programs have the same flags between
# machines, same man pages etc.
,path --prepend "$brew_prefix/opt/coreutils/libexec/gnubin"
export MANPATH="$brew_prefix/opt/coreutils/libexec/gnuman:$MANPATH"

# golang
,path "$brew_prefix/opt/go/libexec/bin/"

# Latex
if [ -d /usr/local/texlive/2023 ]; then
  ,path /usr/local/texlive/2023/bin/universal-darwin/
fi

# Bash completion - use v2 (lazy-loading) if available, fall back to v1.
# Set BASH_COMPLETION_COMPAT_DIR to empty to skip eager loading of
# /opt/homebrew/etc/bash_completion.d/ (~33 files). Most of these tools also
# ship completions in /opt/homebrew/share/bash-completion/completions/ which
# are lazy-loaded on first tab. If tab completion is missing for a command,
# check if it only has a file in etc/bash_completion.d/ and not in
# share/bash-completion/completions/ — if so, symlink it into the latter.
if [[ $- == *i* ]]; then
  export BASH_COMPLETION_COMPAT_DIR=
  if [ -f "$brew_prefix/etc/profile.d/bash_completion.sh" ]; then
    . "$brew_prefix/etc/profile.d/bash_completion.sh"
  elif [ -f "$brew_prefix/etc/bash_completion" ]; then
    . "$brew_prefix/etc/bash_completion"
  fi
fi

function ,finder-pull() {
    # cd to topmost Finder window directory
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}

# Try using this new iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Claude installs here for some reason
,path "${HOME}/.claude/local"
