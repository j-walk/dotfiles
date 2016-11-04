    " Welcome to the wonderful world of my .vimrc
set nocompatible

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'junegunn/vim-easy-align'
Plugin 'eagletmt/neco-ghc'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Valloric/YouCompleteMe'

" Plugin 'junegunn/limelight.vim'
" Plugin 'tpope/vim-fugitive'
" Plugin 'idris-hackers/idris-vim'
" Plugin 'nbouscal/vim-stylish-haskell'
" Plugin 'metakirby5/codi.vim'
" Plugin 'bitc/vim-hdevtools'

call vundle#end()
filetype plugin indent on
syntax on

"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_haskell_checkers = ['hlint']
let g:ycm_semantic_triggers = {'haskell' : ['.']}

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/lib'))
endfunction

map <C-n> :NERDTreeToggle<CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

let mapleader=" "

set encoding=utf-8
set list lcs=eol:¬                " Trailing whitespace, tabs, and newlines
set expandtab                     " Tabs must be spaces
set tabstop=2                     " Tabs must be 2 spaces
set shiftwidth=2
set number                        " Numbered lines
set nowrap
set showcmd                       " CMD line persists past command
set showmatch                     " Highlight grouping matches
set ignorecase                    " ignore case when searching
set incsearch                     " Search as I type
set hlsearch                      " highlight matches
set ruler                         " show the col, line numbers
set smartindent                   " context sensitive indenting
set wildmenu                      " tab completion
set wildmode=list:longest,full
set backspace=indent,eol,start
set scrolloff=10                  " keep 10 lines around the cursor
set fileformats=unix,dos,mac      " support all kinds of lind-endings

colorscheme hipster

highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v',100) " highlight the 80th column

highlight ColorEnd ctermbg=red
call matchadd('ColorEnd', '\s\s*$', 100)

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
