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

function ,tmp {
    if [ -t 0 ]; then
        less /tmp/matt_tmp
    else
        rm /tmp/matt_tmp
        tee /tmp/matt_tmp | less - <&0
    fi
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

function ,kill {
    echo "obsolete. use fzf tab completion with kill"
}

function ,date {
    date -u +"%Y-%m-%dT%H:%M:%S"
}

function ,k-secret-decode {
    kubectl get secret $@ -o go-template='{{range $k,$v := .data}}{{printf "%s: " $k}}{{if not $v}}{{$v}}{{else}}{{$v | base64decode}}{{end}}{{"\n"}}{{end}}'
}
