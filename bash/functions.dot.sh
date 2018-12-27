function ,dotfiles-reload {
    # Reload dotfiles environment
    source "$DOTFILES/activate.sh"
    bind -f "$DOTFILES/inputrc.symlink"
}


# Use fzf to cd into a fasd directory.
function ,cd {
    local dir
    dir=$(fasd -dRl | fzf-tmux --header=",cd" --exact --query="$1" --preview="ls {}") && cd "${dir}" || return 1
}


function ,hist-search {
    history | grep -i "$1" | sed 's/^ *[0-9]* *//' | sort | uniq
}


function ,g {
    rg -C=2 --smart-case $@
}

function ,ag {
    ag "$@" \
        --color \
        --break \
        --context=2 \
        --pager=less \
        --color-line-number=36 \
        --color-path=32 \
        --color-match=1\;4\;34;
}


function ,agg {
    cd "$(git rev-parse --show-toplevel)"
    ,ag "$@"
}


function ,liteshell {
    echo "Loading liteshell..."
    DOTFILES_LITE=true bash
    echo "...exited liteshell"
}


function ,serve {
    python -m SimpleHTTPServer $@ || python -m http.server $@
}


function ,tmp {
    if [ -t 0 ]; then
        less /tmp/matt_tmp
    else
        rm /tmp/matt_tmp
        tee /tmp/matt_tmp | less - <&0
    fi
}


function ,vsshrc {
    CONFIG_PATH=/tmp/vsshrc-config

    vagrant ssh-config "$@" --host vagrant > "$CONFIG_PATH"
    sshrc vagrant -F "$CONFIG_PATH"
}


function ,github-switch-url {
    git='git@github.com:'
    https='https://github.com/'

    url="$(git remote -v | awk '/^origin.*fetch.$/ {print $2}')"
    echo Old url: "$url"

    if echo "$url" | grep -q '^git'
    then
        url="${url/${git}/${https}}"
    else
        url="${url/${https}/${git}}"
    fi

    echo New url: "$url"
    git remote set-url origin "$url"
}


# TODO: remove? This is provided by fzf bash completion.
function ,kill {
    pids=$(ps -ef | sed 1d | fzf-tmux --exact -m --header ",kill" | awk '{print $2}')

    for pid in ${pids}; do

        python -c "int('$pid')" 2>/dev/null || exit  # Exit if we didn't get a PID

        ps -ef | grep "$pid"
        echo

        # -15 is the default signal
        read -p "Enter kill signal for $pid if requored (defaults to 15 SIGTERM): " -r
        if [ -z $REPLY ]; then
            cmd="kill $pid"
        else
            cmd="kill -$REPLY $pid"
        fi

        read -p "Are you sure you want to run command: $cmd (y/n) " -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Exiting."
            exit
        fi

        echo "Sending signal..."
        $cmd
        echo "...done"
    done
}

function ,date {
    python -c 'from __future__ import print_function; import datetime; print(datetime.datetime.now().isoformat())'
}
