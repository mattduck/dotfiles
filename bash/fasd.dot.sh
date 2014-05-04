# NOTE - can see the default aliases by running $(fasd --init posix-alias)
eval "$(fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install)"

alias j="z" # Jump to best match dir
alias jj="zz" # Jump to selected dir
alias v="fasd -f -e vim" # Edit best match file
alias vv="fasd -sia -e vim" # Edit selected file

_fasd_bash_hook_cmd_complete j jj v vv 
