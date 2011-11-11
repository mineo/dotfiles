set runtimepath+=/usr/share/vim
set t_Co=256
set encoding=utf-8
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
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
" automatically indent
set autoindent
" show some more characters
set list
set listchars=tab:»\ ,trail:«,eol:<
set nobackup
set directory-=.
set showcmd
set number
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
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
let NERDTreeWinPos='right'

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

map <F4> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>
" http://bitbucket.org/sjl/gundo.vim/src
map <F5> :GundoToggle<CR>

let mapleader = ","
let g:pep8_map = '<leader>8'

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
" command-t
map <C-t> :CommandT<CR>

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
imap <C-a> <C-x><C-o>
" inoremap ' ''<Left>
" inoremap " ""<Left>
" Open NERDTree by default
"autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p

au FileType python map <F1> \pw
au FileType python map <F6> :!python2 %<CR>
au FileType python map N :cn<CR>
au FileType python map P :cp<CR>
au FileType python set colorcolumn=80
au FileType python set makeprg=pep8\ --repeat\ %
au FileType python set tw=79

au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>

au FileType rst set colorcolumn=80
au FileType rst set tw=79
au FileType rst map <F6> :!rst2pdf %<CR>
