set runtimepath+=/usr/share/vim
set t_Co=256
set encoding=utf-8

"colorscheme synic
colorscheme zenburn
set guifont=Monospace\ 13
" status line
set laststatus=2
"set statusline+=\%h%m%r%w[%{strlen(&ft)?&ft:'none'}] "left side
" set statusline+=%=0x%-14(%l,%c%V%)%<%P "right side
set statusline=%r%y\ [%m%f]\ %{&ff}\ %=CxL:%cx[%l/%L]
syntax on
" automatically indent
set autoindent
" show some more characters
set list
set listchars=tab:»\ ,trail:«
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
" python stuff
let g:pydiction_location = '/usr/share/pydiction/complete-dict'
autocmd FileType python setlocal tw=79 expandtab tabstop=4
" Show matching brackets etc
set showmatch
set foldmethod=indent
" Don't automagically fold everything
set nofoldenable
set matchpairs+=<:>
set backspace=2 "to backspace over linebreaks
set tw=79

map <F1> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>
map gt :bnext!<CR>
map gT :bprev!<CR>
map co ,c 
" Smart way to move btw. windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
" Use the arrows to something usefull
map <right> :bn<cr>
map <left> :bp<cr>

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
" inoremap ' ''<Left>
" inoremap " ""<Left>

au FileType python map <F6> :!python %<CR>
au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>
