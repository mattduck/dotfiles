" Use ; for cmdline. ; usually shows next results for f/F/t/T commands. 
noremap ; :

" Use , to show next results for f/F/t/T commands. Usually shows previous
" results, which is now disabled. 
noremap , ;

" Unbind C-z to stop accidentally stopping process.
" Might put this back in soon.
noremap <C-z> <Nop> 

" Unbind arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>
inoremap <Up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

" Easymotion
noremap <Space> <Nop>
let g:EasyMotion_leader_key = "<Space>"

" Use perl-style regular expressions
nnoremap / /\v
vnoremap / /\v

" Escape insert mode w/jj
inoremap jj <ESC>

" Behave properly on long lines
nnoremap j gj
nnoremap k gk

"""""""""" Leader commands

" :nohl
nnoremap <leader>n :nohl<CR>

" Don't care about most NERDcomment functionality, just want toggle
nnoremap <leader>c :call NERDComment(0,"toggle")<CR>
vnoremap <leader>c :call NERDComment(0,"toggle")<CR>
