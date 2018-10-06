# NOTE - fasd installs some default aliases. I'm not explicitly removing them,
# but I don't really use them either, except for "z".
alias f=",cd"

# NOTE: using the cached method suggested by Github
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# Jump to best match. Use fzf interface if no arg given.
function j () {
    if [ -z "$1" ]; then ,cd; else z $@; fi
}

_fasd_bash_hook_cmd_complete j
