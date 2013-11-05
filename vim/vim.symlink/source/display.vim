set hlsearch    " Highlight search results
set incsearch   " Show search matches as type

set wrap    " Wrap long lines
set textwidth=80
set scrolloff=5     " Scroll page with the cursor 
set cursorline      " Hightlight current line
if exists('+colorcolumn') 
    set colorcolumn=+1     " Boundary for line wrapping
endif 

set showmatch   " Showing matching brackets

set t_Co=16     " Solarized uses 16 colours
let g:CSApprox_verbose_level = 0    " Get rid of CSApprox 16col warnings 

set background=dark
colorscheme solarized
let g:solarized_termcolours=16

if has("syntax")
  syntax on
endif

set ttyfast     " Improves redrawing

set showcmd     " Show commands that typing, eg. 'f'. 
set ruler       " Show column no. in status bar. 
set number      " Line numbers
set noerrorbells
set novisualbell

set cmdheight=2

set wildmenu " Show all autocomplete choices
