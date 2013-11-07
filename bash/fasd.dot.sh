# NOTE - can see the default aliases by running $(fasd --init posix-alias)
eval "$(fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install)"

alias j="z" # Jump to top matching dir
alias jj="zz" # Select dir to jump to
alias v="s -e vim" # Select file to open w/vim
alias lj="sd -e ls" # Select directory to run ls on

_fasd_bash_hook_cmd_complete j jj v lj
