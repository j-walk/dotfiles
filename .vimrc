    " Welcome to the wonderful world of my .vimrc

let mapleader = "\<Space>"

set nocompatible

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'junegunn/vim-easy-align'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'wincent/command-t'
Plugin 'tpope/vim-fugitive'

call vundle#end()
filetype plugin indent on
syntax on

" --{ end of vundle stuff }-- "

let g:syntastic_haskell_checkers = ['hlint']

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

map <C-n> :NERDTreeToggle<CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

set encoding=utf-8
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

nnoremap <Leader>c :set cursorline!<CR>
hi CursorLine   cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white

colorscheme hipster
hi CursorLine term=bold cterm=bold guibg=Grey40
highlight Search ctermbg=black cterm=underline

highlight ColorEnd ctermbg=red
call matchadd('ColorEnd', '\s\s*$', 100)

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

set winwidth=84
set winheight=5
set winminheight=5
set winheight=999

nnoremap <leader><leader> <c-^>
noremap <leader>ss :source ~/.vimrc<cr> :echo "reloaded"<cr>
