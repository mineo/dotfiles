set runtimepath+=/usr/share/vim
filetype off
"
" Vundle
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Plugin 'airblade/vim-rooter'
Plugin 'alfredodeza/coveragepy.vim'
Plugin 'alfredodeza/pytest.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'chriskempson/base16-vim'
Plugin 'dag/vim2hs'
Plugin 'davidhalter/jedi-vim'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'fatih/vim-go'
Plugin 'gmarik/vundle'
Plugin 'honza/vim-snippets'
Plugin 'jmcantrell/vim-virtualenv'
Plugin 'junegunn/vim-pseudocl'
Plugin 'junegunn/vim-oblique'
Plugin 'kien/ctrlp.vim'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'majutsushi/tagbar'
Plugin 'mhinz/vim-startify'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'Shougo/vimproc'
Plugin 'SirVer/ultisnips'
Plugin 'sjl/gundo.vim'
Plugin 'tomasr/molokai'
Plugin 'terryma/vim-expand-region'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tommcdo/vim-centaur'
Plugin 'tommcdo/vim-fugitive-blame-ext'
Plugin 'Townk/vim-autoclose'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'vim-perl/vim-perl'
Plugin 'vim-scripts/kwbdi.vim'
Plugin 'wellle/targets.vim'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-easytags'
Plugin 'ZoomWin'

set t_Co=256
set encoding=utf-8
set bg=dark
set guifont=Consolas\ 13
if has('gui_running')
    colorscheme base16-monokai
    set go=c
    set guicursor+=a:blinkon0
else
    let g:molokai_original=1
    colorscheme molokai
endif

" status line
set laststatus=2
set statusline=%r%y\ [%m%f]\ %{&ff}\ %=CxL:%cx[%l/%L]\ %{fugitive#statusline()}

" airline
" old vim-powerline symbols
let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.branch = '⭠'
let g:airline_symbols.readonly = '⭤'
let g:airline_symbols.linenr = '⭡'
let g:airline_theme='luna'
let g:airline#extensions#whitespace#enabled = 0

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

" Ignore directories
set wildignore+=*/.git/*,*/.hg/*,*/docs/build/*,*/.tox/*,*/build/*,*.o,*.hi,*.pyc,*.pyo

let NERDTreeWinPos = 'right'
let NERDTreeIgnore = ['\.pyc$', '\.o$', '\.hi$']

let mapleader = ","

" easytags
" http://stackoverflow.com/a/16920294/307681
" without this scrolling after :HighlightTags gets really really slow
set re=1
" no warning update update time being too low
let g:easytags_updatetime_warn = 0
"let g:easytags_autorecurse = 1
set tags=./tags;
let g:easytags_dynamic_files = 1
let g:easytags_file = "./tags"

" CtrlP
let g:ctrlp_extensions = ['tag']
let g:ctrlp_cmd= 'CtrlP'
nnoremap <leader>t :CtrlPTag<CR>
nnoremap <leader>bt :CtrlPBufTag<CR>

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

map <F2> :TagbarToggle<CR>
map <F3> :CtrlPBuffer<CR>
map <F4> :NERDTreeToggle<CR>
" http://bitbucket.org/sjl/gundo.vim/src
map <F5> :GundoToggle<CR>

map gt :bnext!<CR>
map gT :bprev!<CR>
map co ,c 
" open a new vertical split, switch to it
nnoremap <leader>w <C-w>v<C-w>l
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l
"
" ultisnips
let g:UltiSnipsUsePythonVersion = 2
let g:UltiSnipsSnippetDirectories = ["UltiSnips"]

" jedi
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 0
let g:jedi#show_call_signatures = 0

" syntastic
let g:syntastic_go_checkers = ["gofmt", "golint", "govet"]
let g:syntastic_python_checkers = ["flake8"]
let g:syntastic_python_flake8_exec = "flake8-python2"

nnoremap <leader>e :Errors<CR>

" omni completion
imap <C-a> <C-x><C-o>

" remap jk to ESC
inoremap jk <ESC>
inoremap kj <ESC>

" ack
nnoremap <leader>a :Ack<space>
nnoremap <leader>A :AckFromSearch<CR>

" easymotion
map <Leader>h <Plug>(easymotion-linebackward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>l <Plug>(easymotion-lineforward)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

" templates
let g:template_dir = "~/.vim/templates"

" Remove trailing whitespace on <leader>S
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

" Quit window on <leader>q
nnoremap <leader>q :q<CR>

" ; is an alias for :
nnoremap ; :

autocmd VimEnter * wincmd p

au FileType python map N :cn<CR>
au FileType python map P :cp<CR>
au FileType python set colorcolumn=80
au FileType python set tw=79

au FileType c map <F6> :!gcc %<CR>
au FileType c map <F7> :!./a.out %<CR>

au FileType rst set colorcolumn=80
au FileType rst set tw=79
au FileType rst map <F6> :!rst2pdf %<CR>

au FileType haskell setlocal omnifunc=necoghc#omnifunc

" musicbrainz-server: *.tt files are template toolkit files including html
au BufNewFile,BufRead *.tt setf tt2html
