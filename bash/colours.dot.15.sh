# ANSI escape codes
Color_Off="\e[0m"
Eblack="\e[0;30m"
Ered="\e[0;31m"
Egreen="\e[0;32m"
Eyellow="\e[0;33m"
Eblue="\e[0;34m"
Epurple="\e[0;35m"
Ecyan="\e[0;36m"
Ewhite="\e[0;37m"
EIblack="\e[0;90m"
EIred="\e[0;91m"
EIgreen="\e[0;92m"
EIyellow="\e[0;93m"
EIblue="\e[0;94m"
EIpurple="\e[0;95m"
EIcyan="\e[0;96m"
EIwhite="\e[0;97m"

# Numbers
Nblack="0"
Nred="1"
Ngreen="2"
Nyellow="3"
Nblue="4"
Npurple="5"
Ncyan="6"
Nwhite="7"
NIblack="8"
NIred="9"
NIgreen="10"
NIyellow="11"
NIblue="12"
NIpurple="13"
NIcyan="14"
NIwhite="15"

# Make system colour variables available, to help differentiate machines in 
# status lines etc.
if command -v lsb_release >/dev/null; then
    if [ -z "$DESKTOP_SESSION" ]; then
        # Server distro
        HOSTCOLOUR=$Egrey
        TMUXCOLOUR=$NIblue
    else
        DISTRO_INFO="$(lsb_release -a)"
        if [[ $DISTRO_INFO == *Ubuntu* ]]; then
            HOSTCOLOUR=$Epurple
            TMUXCOLOUR=$Npurple
        elif [[ $DISTRO_INFO == *Mint* ]]; then
            HOSTCOLOUR=$Egreen
            TMUXCOLOUR=$Ngreen
        else
            # Fallback
            HOSTCOLOUR=$Egrey
            TMUXCOLOUR=$NIblue
        fi
    fi
elif [[ "$(uname -a)" == *Darwin* ]]; then
    # OS X
    HOSTCOLOUR=$Eblue
    TMUXCOLOUR=$Nblue
else
    # Fallback
    HOSTCOLOUR=$Egrey
    TMUXCOLOUR=$NIblue
fi

export HOSTCOLOUR
export TMUXCOLOUR

eval $(dircolors -b $DOTFILES/solarized/dircolors/dircolors.ansi-universal)
