if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# Bash completion lives in the brew directory
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Use coreutils to support solarized colours
alias ls="gls --color=auto"
alias dircolors="gdircolors"
alias sort="gsort"

alias emacs-app="/Applications/Emacs.app/Contents/MacOS/Emacs"

function ,finder-pull() {
    # cd to topmost Finder window directory
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}
