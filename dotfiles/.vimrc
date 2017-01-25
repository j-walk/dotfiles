    " Welcome to the wonderful world of my .vimrc


set nocompatible

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'        " Plugin manager

Plugin 'vim-scripts/vim-auto-save'   " Auto save

Plugin 'Shougo/vimproc.vim'          " VimProc + VimShell for interactive things
Plugin 'Shougo/vimshell.vim'
Plugin 'tpope/vim-dispatch'          " Asynchronously dispatch commands

Plugin 'wincent/command-t'           " Fuzzy file finder
Plugin 'scrooloose/syntastic'        " Style checker

Plugin 'tpope/vim-surround'          " Changing the surround (quotes, parenthesies, etc)
Plugin 'junegunn/vim-easy-align'     " Easy alignment of text objects

" Plugin 'mattn/webapi-vim'            " some kind of web API
" Plugin 'Xuyuanp/nerdtree-git-plugin' " Something I never use
" Plugin 'scrooloose/nerdtree'         " Another thing I never use
" Plugin 'rust-lang/rust.vim'          " I played with this once

call vundle#end()                    " Vundle says this is needed
filetype plugin indent on
syntax on

" --{ end of vundle stuff }-- "

let mapleader = "\<Space>"

let g:syntastic_always_populate_loc_list = 1 " keep the errors in a list
let g:syntastic_auto_loc_list = 1            " always repopulate
let g:syntastic_check_on_open = 1            " populate on open
let g:syntastic_check_on_wq = 0              " no sense in populating on close

let g:auto_save = 1                          " autosave options
let g:auto_save_no_updatetime = 1
let g:auto_save_in_insert_mode = 0

let g:vimshell_prompt = "> "                 " vimshell prompt, like PS1

set statusline+=%#warningmsg#                " Syntastic status line errors
set statusline+=%{syntasticstatuslineflag()}
set statusline+=%*

set directory=$HOME/.vim/swapfiles//

set encoding=utf-8
set expandtab                     " Tabs must be spaces
set tabstop=2                     " Tabs must be 2 spaces
set shiftwidth=2
set number                        " Numbered lines
set nowrap                        " line wrapping is bad
set showcmd                       " CMD line persists past command
set showmatch                     " Highlight grouping matches
set ignorecase                    " ignore case when searching
set incsearch                     " Search as you type
set hlsearch                      " highlight matches
set ruler                         " show the col, line numbers
set smartindent                   " context sensitive indenting
set backspace=indent,eol,start    " backspace works on empty lines
set scrolloff=15                  " keep 10 lines around the cursor
set fileformats=unix,dos,mac      " support all kinds of lind-endings

set textwidth=0 wrapmargin=0

" set winminwidth=80                " sets the minimum width and height of a
" set winwidth=999
" set winheight=999              " à la Gary Bernhardt
" set winminheight=5             " split

colorscheme hipster               " The Hipster color scheme, thanks Conner

hi CursorLine cterm=underline ctermbg=NONE
hi Search     cterm=underline ctermbg=NONE

xmap ga <Plug>(EasyAlign)
nmap ga <plug>(easyalign)

  " return to other file
nnoremap <leader><leader> <c-^>
  " reload .virmc
nnoremap <leader>ss :source ~/.vimrc<cr> :echo "reloaded"<cr>

  " jump to syntastic errors
nnoremap <leader>n :lnext<cr>
nnoremap <leader>p :lprev<cr>

nnoremap <leader>d :Dispatch 

nnoremap <silent> <C-d> :lclose<CR>:bdelete<CR>

