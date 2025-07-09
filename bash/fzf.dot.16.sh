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

# Empty completion trigger causes this to work on <TAB> rather than *<TAB>
export FZF_DEFAULT_OPTS='-0 --cycle --inline-info --border --color="16,border:8,bg+:-1"'
export FZF_COMPLETION_TRIGGER="*"
export FZF_COMPLETION_OPTS='--exact --height 20 --cycle -0 --border --color="16,border:8,bg+:-1" --multi'

# [2021-05-16] fzf-tab-completion setup. This provides proper fzf completion for
# all bash tab complete candidates.
if [ -f "$DOTFILES/fzf-tab-completion/bash/fzf-bash-completion.sh" ]; then
    source "$DOTFILES/fzf-tab-completion/bash/fzf-bash-completion.sh"
    # Only bind keys in interactive shells to avoid warnings
    if [[ $- == *i* ]]; then
        bind -x '"\t": fzf_bash_completion'
        # Rebind the default completion in case the fzf version doesn't work
        bind '"\C-k": complete'
    fi
fi
