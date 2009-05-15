"set t_Co=256
"colorscheme mustang
"colorscheme slate
if has('gui_running')
	colorscheme digerati
else
	colorscheme digerati
endif

syntax on
set showcmd
set number
set hlsearch
set smartindent
set tabstop=4
set shiftwidth=4
set ignorecase
filetype plugin indent on 
set showmatch

map <C-q> :quit<CR>
map <F1> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
imap < <><Left>
inoremap ' ''<Left>
inoremap " ""<Left>
