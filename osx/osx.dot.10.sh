if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# Use GNU coreutils - it's easier if programs have the same flags between
# machines, same man pages etc.
,path-add --prepend "/usr/local/opt/coreutils/libexec/gnubin"
,path-add --prepend "/usr/local/opt/coreutils/libexec/gnuman"

# Use Brew instead of system Ruby
,path-add "/usr/local/opt/ruby/bin"

# Add TeX to path (for org-mode PDF exporting)
,path-add "/usr/local/texlive/2014/bin/universal-darwin"

# git __ps1 at work
git_prompt_path=~/bin/git-prompt.sh
if [ -f "$git_prompt_path" ]; then
    source "$git_prompt_path"
fi

# ...and completion
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
alias vi="vim" # Otherwise vi will point to /usr/bin, and vim to the brew dir.

function ,finder-pull() {
    # cd to topmost Finder window directory
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}
