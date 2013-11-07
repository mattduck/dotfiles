# Solarized ANSI
Anone="\e[00m"
Abase02="\e[00;30m"
Abase01="\e[01;30m"
Ared="\e[00;31m"
Aorange="\e[01;31m"
Agreen="\e[00;32m"
Abase01="\e[01;32m"
Ayellow="\e[00;33m"
Abase00="\e[01;33m"
Ablue="\e[00;34m"
Abase0="\e[01;34m"
Amagenta="\e[00;35m"
Aviolet="\e[01;35m"
Acyan="\e[00;36m"
Abase1="\e[01;36m"
Abase2="\e[00;37m"
Abase3="\e[01;37m"

# Solarized numbers
Nbase03=8
Nbase02=0
Nbase01=10
Nbase00=11
Nbase0=12
Nbase1=14
Nbase2=7
Nbase3=15
Nyellow=3
Norange=9
Nred=1
Nmagenta=5
Nviolet=13
Nblue=4
Ncyan=6
Ngreen=2

# Make system colour variables available, to help differentiate machines in 
# status lines etc.
if command -v lsb_release >/dev/null; then
    DISTRO_INFO="$(lsb_release -a)"
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
