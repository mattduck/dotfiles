# Assumes that fzf is installed

if [ $(command -v brew) ]; then

    function fzf-prefix() {
        echo "$(brew --prefix)/opt/fzf"
    }
    
    # This is basically what happens in ~/.fzf.bash, which is provided by fzf.
    ,path $(fzf-prefix)/bin
    if [[ ! "$MANPATH" == *"$(fzf-prefix)/man"* && -d "$(fzf-prefix)/man" ]]; then
        export MANPATH="$MANPATH:$(fzf-prefix)/man"
    fi
    source "$(fzf-prefix)/shell/completion.bash"
fi

export FZF_DEFAULT_OPTS="-0 --cycle --inline-info"
export FZF_COMPLETION_TRIGGER="*"
export FZF_COMPLETION_OPTS='--exact'
