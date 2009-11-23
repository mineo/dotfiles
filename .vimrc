set runtimepath+=/usr/share/vim
set t_Co=256
set encoding=utf-8

colorscheme mustang

"status line
set statusline+=\%h%m%r%w[%{strlen(&ft)?&ft:'none'}] "left side
set statusline+=%=0x%-14(%l,%c%V%)%<%P "right side

syntax on
set nobackup
set directory-=. 
set showcmd
set number
set smartindent
set tabstop=4
set shiftwidth=4
set ignorecase
set hlsearch
set incsearch
filetype plugin indent on 
let g:pydiction_location = '/usr/share/pydiction/complete-dict'
set showmatch
set foldmethod=indent

set backspace=2 "to backspace over linebreaks


map <C-q> :quit<CR>
map <F1> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
inoremap ' ''<Left>
inoremap " ""<Left>

au FileType python map <F6> :!python %<CR>
