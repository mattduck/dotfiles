# [2024-04] I used to use fasd as a "j" command, but it's now archived on github
# and removed from brew, so using zoxide instead, as it has a lot of community
# activity behind it.
function j () {
    if [ -z "$1" ]; then zi; else z $@; fi
}

if [ "$(command -v zoxide)" ]; then
    eval "$(zoxide init bash)"
fi
