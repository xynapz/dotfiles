" Plugin Manager: vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')

" Modus themes (vim version)
Plug 'c9rgreen/vim-colors-modus'


" File-tree
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

call plug#end()

" General settings
set nocompatible
set encoding=utf-8
set backspace=indent,eol,start
set history=1000
set undolevels=1000

" Always use system/global clipboard
set clipboard=unnamed,unnamedplus

" Wayland Clipboard Support (wl-clipboard)
let g:clipboard = {
            \   'name': 'wl-clipboard',
            \   'copy': {
            \      '+': ['wl-copy', '--trim-newline', '--type', 'text/plain'],
            \      '*': ['wl-copy', '--trim-newline', '--type', 'text/plain'],
            \    },
            \   'paste': {
            \      '+': ['wl-paste', '--no-newline'],
            \      '*': ['wl-paste', '--no-newline'],
            \   },
            \   'cache_enabled': 1,
            \ }

" UI and display
set number
set relativenumber
set showcmd
set showmatch
set ruler
set laststatus=2
set wildmenu
set wildmode=longest:full,full
set signcolumn=yes
set cmdheight=2
set updatetime=300
set shortmess+=c

" Search
set hlsearch
set incsearch
set ignorecase
set smartcase

" Indentation
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set wrap
set linebreak

" File handling
set autoread
set noswapfile
set nobackup
set undofile
set undodir=~/.vim/undo

if has('mouse')
    set mouse=a
endif

set visualbell
set noerrorbells
set lazyredraw
set ttyfast

" Colors
syntax enable
set termguicolors
set background=dark

" Install Modus Vivendi (via vim-plug)
" Install Modus Vivendi (via vim-plug)
try
    colorscheme modus
catch
    colorscheme default
endtry

" Cursor / highlight adjustments
highlight CursorLine cterm=NONE ctermbg=236 guibg=#2d2d2d
highlight CursorColumn cterm=NONE ctermbg=236 guibg=#2d2d2d
highlight LineNr ctermfg=grey guifg=#5c6370
highlight CursorLineNr cterm=bold ctermfg=yellow guifg=#e5c07b

" Folding
set foldmethod=syntax
set foldlevelstart=10
set foldnestmax=10

" Leader key
let mapleader = " "

" Key mappings
nnoremap <leader>h :nohlsearch<CR>

nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>x :x<CR>

" Buffers
nnoremap <leader>n :bnext<CR>
nnoremap <leader>p :bprev<CR>
nnoremap <leader>d :bdelete<CR>

" Windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>v :vsplit<CR>
nnoremap <leader>s :split<CR>

" Move lines
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

nnoremap <space> za
vnoremap < <gv
vnoremap > >gv

" Autocmds
autocmd FileType c,cpp setlocal tabstop=8 shiftwidth=8 noexpandtab
autocmd FileType python setlocal tabstop=4 shiftwidth=4 expandtab
autocmd FileType javascript,html,css,json setlocal tabstop=2 shiftwidth=2 expandtab
autocmd FileType make setlocal tabstop=8 shiftwidth=8 noexpandtab

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

" Ensure undo directory exists
if !isdirectory($HOME."/.vim/undo")
    call mkdir($HOME."/.vim/undo", "p", 0700)
endif

" Statusline
set statusline=%#PmenuSel#\ %f\ %#LineNr#\ %m%r%h%w\ %=%#CursorColumn#\ %y\ %#PmenuSel#\ %l:%c\ %#LineNr#\ %p%%\

" NERDTree keybinds
nnoremap <leader>e :NERDTreeToggle<CR>

" Focus current file in tree
nnoremap <leader>f :NERDTreeFind<CR>
