# NOTE - fasd installs some default aliases. I'm not explicitly removing them,
# but I don't really use them either, except for "z".
eval "$(fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install)"

alias f=",cd"  # TODO fix so I can define this in aliases and fasd doesn't override

# Jump to best match. Use fzf interface if no arg given.
function j () {
    if [ -z "$1" ]; then ,cd; else z $@; fi
}

_fasd_bash_hook_cmd_complete j
