function ,git-cd() {
    # cd to top level of git directory
    cd "$(git rev-parse --show-toplevel)"
}

# Usually my PAGER setting contains 'less -R' (plus some other flags), but this
# causes git-delta's hyperlinks to display wrong in the terminal.
# The workaround is to use 'less -r'. See https://github.com/dandavison/delta/issues/362.
export DELTA_PAGER='less -rX'
