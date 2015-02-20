autocmd FileType python setlocal textwidth=79   " PEP 8
"autocmd FileType python compiler flake8         " :pylint 
"let g:pylint_onwrite = 0                        " Disable pylint call when saving
"let g:pylint_inline_highlight = 0               " No pylint highlighted lines

autocmd FileType python source $DOTFILES/vim/vim.symlink/bundle/jpythonfold.vim/syntax/jpythonfold.vim
