set runtimepath+=/usr/share/vim
set t_Co=256
set encoding=utf-8
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
colorscheme mustang
set guifont=Monospace\ 13
" status line
set laststatus=2
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
set expandtab
set ignorecase
set hlsearch
set incsearch
filetype plugin indent on
" Show matching brackets etc
set showmatch
set foldmethod=indent
" Don't automagically fold everything
set nofoldenable
set matchpairs+=<:>
set backspace=2 "to backspace over linebreaks
let NERDTreeWinPos='right'
set tags+=~/.vim/tags/cpp
set tags+=~/.vim/tags/libnet
set tags+=~/.vim/tags/jni
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview
map <F12> :!ctags -R --sort=yes --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

map <F4> :NERDTreeToggle<CR>
map <F2> :TlistToggle<CR>
" http://bitbucket.org/sjl/gundo.vim/src
map <F5> :GundoToggle<CR>
map gt :bnext!<CR>
map gT :bprev!<CR>
map co ,c 
" compview:
map / \v

imap { {}<Left>
imap ( ()<Left>
imap [ []<Left>
" inoremap ' ''<Left>
" inoremap " ""<Left>
" Open NERDTree by default
autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p

au FileType python map <F6> :!python2 %<CR>
au FileType python set colorcolumn=80
au FileType python map <F1> \pw
au FileType python set tw=79

au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>

au FileType rst set colorcolumn=80
au FileType rst set tw=79
au FileType rst map <F6> :!rst2pdf %<CR>
