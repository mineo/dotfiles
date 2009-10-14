set runtimepath+=/usr/share/vim
set t_Co=256
colorscheme mustang

syntax on
set nobackup
set directory-=. 
set showcmd
set number
set hlsearch
set smartindent
set tabstop=4
set shiftwidth=4
set ignorecase
filetype plugin indent on 
set showmatch
set foldmethod=indent
set backspace=2 "to backspace over linebreaks

let python_highlight_all = 1

map <C-q> :quit<CR>
map <F1> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
imap < <><Left>
inoremap ' ''<Left>
inoremap " ""<Left>

au FileType python map <F6> :!python %<CR>
