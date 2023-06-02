" [2023-06-02] Some basic neovim configuration. I'm not sure how much the
" defaults differ from vim, so just setting a few bindings that I actually
" use regularly.

" Set leader to space
nnoremap <Space> <Nop>
let mapleader = "\<Space>"

" Use ; for cmdline. ; usually shows next results for f/F/t/T commands.
noremap ; :

" Use , to show next results for f/F/t/T commands. Usually shows previous
" results, which is now disabled.
noremap , ;

" Avoid having to press ESC
inoremap jj <ESC>:w<CR>
inoremap jk <ESC>

" Behave properly on long lines
nnoremap j gj
nnoremap k gk

" Strip trailing whitespace and save
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>:w<CR>
nnoremap <leader>w :w<CR>

" Splitscreen window management
nnoremap <C-w>c :tabnew<CR>
nnoremap <C-w>n :tabnext<CR>
nnoremap <C-w>p :tabprevious<CR>
nnoremap <C-w>! :tabedit %<CR>
nnoremap <C-w>X :tabclose

nnoremap <C-w>% :vsplit<CR>
nnoremap <C-w>" :split<CR>
nnoremap <C-w>x :q!
nnoremap <C-w>z :only<CR>
nnoremap <C-w><C-h> <C-w><
nnoremap <C-w><C-j> <C-w>-
nnoremap <C-w><C-k> <C-w>+
nnoremap <C-w><C-l> <C-w>>
nnoremap <C-w><Space> <C-w>=

nnoremap <C-w>1 1gt
nnoremap <C-w>2 2gt
nnoremap <C-w>3 3gt
nnoremap <C-w>4 4gt
nnoremap <C-w>5 5gt
nnoremap <C-w>6 6gt
nnoremap <C-w>7 7gt
nnoremap <C-w>8 8gt
nnoremap <C-w>9 9gt