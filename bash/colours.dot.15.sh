# Solarized ANSI
export Anone="\e[00m"
export Abase02="\e[00;30m"
export Abase01="\e[01;30m"
export Ared="\e[00;31m"
export Aorange="\e[01;31m"
export Agreen="\e[00;32m"
export Abase01="\e[01;32m"
export Ayellow="\e[00;33m"
export Abase00="\e[01;33m"
export Ablue="\e[00;34m"
export Abase0="\e[01;34m"
export Amagenta="\e[00;35m"
export Aviolet="\e[01;35m"
export Acyan="\e[00;36m"
export Abase1="\e[01;36m"
export Abase2="\e[00;37m"
export Abase3="\e[01;37m"

# Solarized numbers
export Nbase03=8
export Nbase02=0
export Nbase01=10
export Nbase00=11
export Nbase0=12
export Nbase1=14
export Nbase2=7
export Nbase3=15
export Nyellow=3
export Norange=9
export Nred=1
export Nmagenta=5
export Nviolet=13
export Nblue=4
export Ncyan=6
export Ngreen=2

# Make system colour variables available, to help differentiate machines in 
# status lines etc.
if command -v lsb_release >/dev/null; then
    DISTRO_INFO=$(lsb_release -a 2>/dev/null)
    if [[ $DISTRO_INFO == *Ubuntu* ]]; then
        HOST_COLOUR_NUM=$Nviolet
        HOST_COLOUR_ANSI=$Aviolet
    elif [[ $DISTRO_INFO == *Mint* ]]; then
        HOST_COLOUR_NUM=$Ncyan
        HOST_COLOUR_ANSI=$Acyan
    else
        # Fallback
        HOST_COLOUR_NUM=$Nbase0
        HOST_COLOUR_ANSI=$Abase0
    fi
elif [[ "$(uname -a)" == *Darwin* ]]; then
    # OS X
    HOST_COLOUR_NUM=$Nblue
    HOST_COLOUR_ANSI=$Ablue
else
    # Fallback
    HOST_COLOUR_NUM=$Nbase0
    HOST_COLOUR_ANSI=$Abase0
fi

export HOST_COLOUR_NUM
export HOST_COLOUR_ANSI

eval $(dircolors -b $DOTFILES/solarized/dircolors/dircolors.ansi-universal)
