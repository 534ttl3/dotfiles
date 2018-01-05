" Vundle requires those 2 lines at the top of .vimrc
set nocompatible
filetype off

" ----- Vundle plugins ------
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" UltiSnips 
Plugin 'SirVer/ultisnips'

" vim-snippets
Plugin 'honza/vim-snippets'

" some color-scheme plugins
" Plugin 'jnurmine/Zenburn'
" Plugin 'altercation/vim-colors-solarized'

" Jedi
Plugin 'davidhalter/jedi-vim'

" Python folding
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492
set nofoldenable

" Bundle 'Valloric/YouCompleteMe'
" let g:ycm_autoclose_preview_window_after_completion=1

call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" Rebind <Leader> key
let mapleader = ","



" -------- settings of loaded plugins ---------

" Ultisnips
" Trigger configuration. Do not use <tab> if you use 
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips', 'UltiSnips']

" Jedi
" let g:jedi#auto_initialization = 0
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "left"
" let g:jedi#popup_on_dot = 0
" let g:jedi#popup_select_first = 0
" let g:jedi#show_call_signatures = "1"

" NOTE: subject to change!

" let g:jedi#goto_command = "<leader>d" 
" you can jump back to the original position using 
" vim's typical command C-o
" let g:jedi#goto_assignments_command = "<leader>g"
" let g:jedi#goto_definitions_command = ""
" let g:jedi#documentation_command = "K"
" let g:jedi#usages_command = "<leader>n"
" let g:jedi#completions_command = "<C-Space>"
" let g:jedi#rename_command = "<leader>r"
" let g:jedi#completions_enabled = 0

" I don't want the docstring window to popup during completion
" autocmd FileType python setlocal completeopt-=preview




" ------ Not plugin related settings ------

syntax on

" press F9 to execute current python buffer
" nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

set encoding=utf-8

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

colorscheme murphy

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" make backspace key work properly in insert mode
set backspace=indent,eol,start

" Showing line numbers and length
set number  " show line numbers
set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
" draw a vertical column (to indicate long lines)
set colorcolumn=80
" highlight ColorColumn ctermbg=233
