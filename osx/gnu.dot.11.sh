if [[ "$(uname -a)" != *Darwin* ]]; then return; fi

# Use coreutils to support solarized colours
alias ls="gls --color=auto"
alias dircolors="gdircolors"
alias sort="gsort"

