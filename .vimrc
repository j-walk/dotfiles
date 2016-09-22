    " Welcome to the wonderful world of my .vimrc
set nocompatible

filetype off

set runtimepath=~/.vim,$VIM/vimfiles,$VIMRUNTIME

syntax on

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'metakirby5/codi.vim'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'bitc/vim-hdevtools'
Plugin 'junegunn/vim-easy-align'
Plugin 'idris-hackers/idris-vim'

let g:validator_haskell_checkers = ['hlint']

call vundle#end()
filetype plugin indent on


xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

let mapleader=" "

set encoding=utf-8
set list lcs=eol:¬,tab:»»,trail:_ " Trailing whitespace, tabs, and newlines
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

