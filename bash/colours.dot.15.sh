# Solarized ANSI
export ANSIreset="\[$(tput sgr0)\]"
export ANSInone="\[\e[00m\]"
export ANSIbase02="\[\e[00;30m\]"
export ANSIbase01="\[\e[01;30m\]"
export ANSIred="\[\e[00;31m\]"
export ANSIorange="\[\e[01;31m\]"
export ANSIgreen="\[\e[00;32m\]"
export ANSIbase01="\[\e[01;32m\]"
export ANSIyellow="\[\e[00;33m\]"
export ANSIbase00="\[\e[01;33m\]"
export ANSIblue="\[\e[00;34m\]"
export ANSIbase0="\[\e[01;34m\]"
export ANSImagenta="\[\e[00;35m\]"
export ANSIviolet="\[\e[01;35m\]"
export ANSIcyan="\[\e[00;36m\]"
export ANSIbase1="\[\e[01;36m\]"
export ANSIbase2="\[\e[00;37m\]"
export ANSIbase3="\[\e[01;37m\]"

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
        HOST_COLOUR_ANSI=$ANSIviolet
    else
        # Fallback
        HOST_COLOUR_NUM=$Nbase0
        HOST_COLOUR_ANSI=$ANSIbase0
    fi
elif [[ "$(uname -a)" == *Darwin* ]]; then
    # OS X
    HOST_COLOUR_NUM=$Nblue
    HOST_COLOUR_ANSI=$ANSIblue
else
    # Fallback
    HOST_COLOUR_NUM=$Nbase0
    HOST_COLOUR_ANSI=$ANSIbase0
fi

export HOST_COLOUR_NUM
export HOST_COLOUR_ANSI

eval $(dircolors -b $DOTFILES/solarized/dircolors/dircolors.ansi-universal)
