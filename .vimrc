    " Welcome to the wonderful world of my .vimrc

colorscheme hipster

set nocompatible

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'junegunn/vim-easy-align'

Plugin 'scrooloose/nerdtree'

Plugin 'wincent/command-t'

Plugin 'lervag/vimtex'

Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'
Plugin 'Xuyuanp/nerdtree-git-plugin'

Plugin 'Shougo/vimproc.vim'

call vundle#end()
filetype plugin indent on
syntax on

" --{ end of vundle stuff }-- "

let mapleader = "\<Space>"

let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

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

set winwidth=84                   " sets the minimum width and height of a
set winheight=5                   " split
set winminheight=5                " à la Gary Bernhardt
set winheight=999

hi CursorLine cterm=underline ctermbg=black
hi Search ctermbg=black cterm=underline
hi ColorEnd ctermbg=red

map <C-n> :NERDTreeToggle<CR>
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
nnoremap <Leader>c :set cursorline!<CR>
nnoremap <leader><leader> <c-^>
nnoremap <leader>ss :source ~/.vimrc<cr> :echo "reloaded"<cr>

call matchadd('ColorEnd', '\s\s*$', 100)

autocmd StdinReadPre * let s:std_in=1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
