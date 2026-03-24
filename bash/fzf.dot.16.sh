# Assumes that fzf is installed

if [ $(command -v brew) ]; then

    _fzf_prefix="/opt/homebrew/opt/fzf"

    # This is basically what happens in ~/.fzf.bash, which is provided by fzf.
    ,path "$_fzf_prefix/bin"
    if [[ ! "$MANPATH" == *"$_fzf_prefix/man"* && -d "$_fzf_prefix/man" ]]; then
        export MANPATH="$MANPATH:$_fzf_prefix/man"
    fi
    # NOTE: fzf's completion.bash eagerly wraps dozens of commands (~500ms).
    # Skipped because fzf-tab-completion (below) handles tab completion instead.
    # source "$_fzf_prefix/shell/completion.bash"
fi

# Empty completion trigger causes this to work on <TAB> rather than *<TAB>
export FZF_DEFAULT_OPTS='-0 --cycle --inline-info --border --color="16,border:8,bg+:-1"'
export FZF_COMPLETION_TRIGGER="*"
export FZF_COMPLETION_OPTS='--exact --height 20 --cycle -0 --border --color="16,border:8,bg+:-1" --multi'

# [2021-05-16] fzf-tab-completion setup. This provides proper fzf completion for
# all bash tab complete candidates. Deferred until first tab press.
if [[ $- == *i* ]]; then
    _fzf_tab_completion_lazy() {
        source "$DOTFILES/fzf-tab-completion/bash/fzf-bash-completion.sh"
        bind -x '"\t": fzf_bash_completion'
        bind '"\C-k": complete'
        fzf_bash_completion
    }
    bind -x '"\t": _fzf_tab_completion_lazy'
    bind '"\C-k": complete'
fi
