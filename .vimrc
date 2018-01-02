" setup using these instructions: https://realpython.com/blog/python/vim-and-python-a-match-made-in-heaven/

set encoding=utf-8

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable folding, type za to fold/unfold
set foldmethod=indent
set foldlevel=99
" Enable folding with the spacebar
nnoremap <space> za

set nocompatible
filetype off 

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

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips', 'UltiSnips']

" correct folding of code
Plugin 'tmhedberg/SimpylFold'

" I want to see docstrings for folded code
let g:SimpylFold_docstring_preview=1

" best auto-complete for python 
Bundle 'Valloric/YouCompleteMe'
" make autocomplete window go away after done
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

Plugin 'jnurmine/Zenburn'
Plugin 'altercation/vim-colors-solarized'

call togglebg#map("<F5>")

set tw=0

" press F9 to execute current python buffer
nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

