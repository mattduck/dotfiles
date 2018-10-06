# Assumes that fzf is installed

function fzf-prefix() {
    echo "$(brew --prefix)/opt/fzf"
}

# This is basically what happens in ~/.fzf.bash, which is provided by fzf.
,path $(fzf-prefix)/bin
if [[ ! "$MANPATH" == *"$(fzf-prefix)/man"* && -d "$(fzf-prefix)/man" ]]; then
    export MANPATH="$MANPATH:$(fzf-prefix)/man"
fi

export FZF_DEFAULT_OPTS="-0 --color=bg:8,fg:12,hl:15,bg+:0,fg+:12,hl+:15,info:6,prompt:6,pointer:1,marker:1,spinner:5,header:3 --cycle --inline-info"

source "$(fzf-prefix)/shell/completion.bash"
export FZF_COMPLETION_TRIGGER="*"
export FZF_COMPLETION_OPTS='--exact'

# Ensure fzf completion can work with git g alias
complete -o bashdefault -o default -o nospace -F _fzf_path_completion g
