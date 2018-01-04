" setup using these instructions: https://realpython.com/blog/python/vim-and-python-a-match-made-in-heaven/

set encoding=utf-8

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

colorscheme murphy

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set backspace=indent,eol,start

" Rebind <Leader> key
" I like to have it here becuase it is easier to reach than the default and
let mapleader = ","


" Showing line numbers and length
set number  " show line numbers
set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
" highlight ColorColumn ctermbg=233


set nocompatible
filetype off 
filetype plugin indent on
syntax on

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Track the engine.
Plugin 'SirVer/ultisnips'
" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

call vundle#end()
filetype plugin indent on

" Trigger configuration. Do not use <tab> if you use 
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips', 'UltiSnips']

" correct folding of code
" Plugin 'tmhedberg/SimpylFold'

" I want to see docstrings for folded code
" let g:SimpylFold_docstring_preview=1

" best auto-complete for python 
" Bundle 'Valloric/YouCompleteMe'
" make autocomplete window go away after done
" let g:ycm_autoclose_preview_window_after_completion=1

Plugin 'jnurmine/Zenburn'
Plugin 'altercation/vim-colors-solarized'

call togglebg#map("<F5>")

set tw=0

" press F9 to execute current python buffer
" nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

" Python folding
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492
set nofoldenable

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile
