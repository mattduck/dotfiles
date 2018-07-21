# Assumes that fzf is installed

function fzf-prefix() {
    echo "$(brew --prefix)/opt/fzf"
}

# This is basically what happens in ~/.fzf.bash, which is provided by fzf.
,path $(fzf-prefix)/bin
if [[ ! "$MANPATH" == *"$(fzf-prefix)/man"* && -d "$(fzf-prefix)/man" ]]; then
    export MANPATH="$MANPATH:$(fzf-prefix)/man"
fi

# - NOTE: I don't source the completion (as don't like the **TAB usage).
#
# - TODO: Set up the keybindings, but ONLY for C-t. I don't want C-r to be
#   overridden - I can make a separate command for that and for the cd one.
source "$(fzf-prefix)/shell/completion.bash"

export FZF_DEFAULT_OPTS="-0 --color=bg:8,fg:12,hl:15,bg+:0,fg+:12,hl+:15,info:6,prompt:6,pointer:1,marker:1,spinner:5,header:3 --cycle --inline-info"
export FZF_COMPLETION_TRIGGER="'" # Easy to reach, doesn't clash with anything
export FZF_COMPLETION_OPTS='--exact'
