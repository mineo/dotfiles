set runtimepath+=/usr/share/vim

set t_Co=256
set encoding=utf-8
set bg=dark
set go=c
set guifont=Consolas\ 13
colorscheme molokai

" status line
set laststatus=2
set statusline=%r%y\ [%m%f]\ %{&ff}\ %=CxL:%cx[%l/%L]

" highlight the line the cursor's in
set cursorline

syntax on
" automatically indent
set autoindent
" show some more characters
set list
set listchars=tab:»\ ,trail:«,eol:<
set nobackup
set directory-=.
set showcmd
set relativenumber
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set ignorecase
set hlsearch
set incsearch
filetype plugin indent on
" Show matching brackets etc
set showmatch
set foldmethod=indent
set foldminlines=7
set foldenable
set matchpairs+=<:>
set backspace=2 "to backspace over linebreaks
set linespace=2
set wildmenu

" Ignore directories
set wildignore+=*/.git/*,*/.hg/*,*/docs/build/*,*/.tox/*,*/build/*,*.o,*.hi,*.pyc,*.pyo

let mapleader = ","

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

map gt :bnext!<CR>
map gT :bprev!<CR>
map co ,c 

" open a new vertical split, switch to it
nnoremap <leader>w <C-w>v<C-w>l
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" open splits where expected
set splitbelow
set splitright

" omni completion
imap <C-a> <C-x><C-o>

" remap jk to ESC
inoremap jk <ESC>
inoremap kj <ESC>

" Remove trailing whitespace on <leader>S
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

" Quit window on <leader>q
nnoremap <leader>q :q<CR>

" ; is an alias for :
nnoremap ; :

autocmd VimEnter * wincmd p

au FileType python set colorcolumn=80
au FileType python set tw=79

au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>

au FileType rst set colorcolumn=80
au FileType rst set tw=79
au FileType rst map <F6> :!rst2pdf %<CR>