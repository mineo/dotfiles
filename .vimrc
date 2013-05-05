set runtimepath+=/usr/share/vim
filetype off
"
" Vundle
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'alfredodeza/coveragepy.vim'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'alfredodeza/pytest.vim'
Bundle 'davidhalter/jedi-vim'
Bundle 'gmarik/vundle'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'kien/ctrlp.vim'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Lokaltog/vim-powerline'
Bundle 'majutsushi/tagbar'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'SirVer/ultisnips'
Bundle 'sjl/gundo.vim'
Bundle 'tomasr/molokai'
Bundle 'terryma/vim-expand-region'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'xolox/vim-easytags'
Bundle 'ZoomWin'

set t_Co=256
set encoding=utf-8
let g:molokai_original=1
colorscheme molokai
set guifont=Consolas\ 14
if has('gui_running')
    set go=c
    set guicursor+=a:blinkon0
endif

" status line
set laststatus=2
set statusline=%r%y\ [%m%f]\ %{&ff}\ %=CxL:%cx[%l/%L]\ %{fugitive#statusline()}
syntax on
let g:Powerline_symbols='fancy'
" automatically indent
set autoindent
" show some more characters
set list
set listchars=tab:»\ ,trail:«,eol:<
set nobackup
set directory-=.
set showcmd
set nonumber
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
set foldenable
set matchpairs+=<:>
set backspace=2 "to backspace over linebreaks

" Ignore directories
set wildignore+=*/.git/*,*/.hg/*,*/doc/*,*/docs/*,*/.tox/*,*/build/*

let NERDTreeWinPos = 'right'
let NERDTreeIgnore = ['\.pyc$']

" easytags
" always highlight
let g:easytags_always_enabled = 1
" no warning update update time being too low
let g:easytags_updatetime_autodisable = 1
"let g:easytags_autorecurse = 1
set tags=./tags;
let g:easytags_dynamic_files = 1
let g:easytags_file = "./tags"

" CtrlP
let g:ctrlp_extensions = ['tag']

" tags
nmap <C-b> :po<CR>

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

map <F1> <Plug>TaskList
map <F4> :NERDTreeToggle<CR>
map <F2> :TagbarToggle<CR>
map <F3> :CtrlPBuffer<CR>
" http://bitbucket.org/sjl/gundo.vim/src
map <F5> :GundoToggle<CR>

let mapleader = ","

map gt :bnext!<CR>
map gT :bprev!<CR>
map co ,c 
" open a new vertical split, switch to it
nnoremap <leader>w <C-w>v<C-w>l
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l
" compview:
map / ,v
" ultisnips
let g:UltiSnipsUsePythonVersion = 2
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "snippets"]

" jedi
let g:jedi#use_tabs_not_buffers = 0

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
imap <C-a> <C-x><C-o>
" ack
nnoremap <leader>a :Ack<space>
" inoremap ' ''<Left>
" inoremap " ""<Left>
" Open NERDTree by default
"autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p

au FileType python map N :cn<CR>
au FileType python map P :cp<CR>
au FileType python set colorcolumn=80
au FileType python set tw=79

au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>
au FileType c map <C-b> :po<CR>

au FileType rst set colorcolumn=80
au FileType rst set tw=79
au FileType rst map <F6> :!rst2pdf %<CR>

au FileType taskpaper setlocal foldmethod=indent
