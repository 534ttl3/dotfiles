" Vundle requires those 2 lines at the top of .vimrc
set nocompatible
filetype off

" ----- Vundle plugins ------
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim' 
" vim-snippets
Plugin 'honza/vim-snippets'

" UltiSnips 
Plugin 'SirVer/ultisnips'
   
" Jedi
Plugin 'davidhalter/jedi-vim'

" autopep8 for python
Plugin 'tell-k/vim-autopep8'

" Nerdtree
Plugin 'scrooloose/nerdtree'

" Nerdtree execute file in gnome standard program
Plugin 'ivalkeen/nerdtree-execute'

" Syntastic (syntax checking) for python
Plugin 'vim-syntastic/syntastic'

" vim-airline (beautiful status line)
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" CtrlP
Plugin 'kien/ctrlp.vim'

" vimux, call tmux command from inside vim and run in it in seperate pane
Plugin 'benmills/vimux'

" vim-conda - STATUS: currently not used, sice my worklflow is to activate 
" a virtualenv in a seperate shell and I don't use any code-completion or 
" linting tools that can reference the python packages in that virtualenv
" enables to switch the python version that vim uses
" within it's running process (not :!python)
" Plugin 'cjrh/vim-conda'

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

" ------ other non-Vundle plugins ------
" Python folding
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492
set nofoldenable


" ------ general vim settings (not plugin-related) ------

" Rebind <Leader> key
let mapleader = ","

" Nerdtree rebind
nmap <leader>t :NERDTree<cr>

" be able to leave buffer without saving (hide buffer)
set hidden

syntax on

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

set encoding=utf-8

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" make backspace key work properly in insert mode
set backspace=indent,eol,start

colorscheme murphy

" Showing line numbers and length
set number  " show line numbers
set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
" draw a vertical column (to indicate long lines)
set colorcolumn=80
" highlight ColorColumn ctermbg=233

" navigate splits more efficiently
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" quickly cycle through buffers
:nnoremap <leader>n :bnext<CR>
:nnoremap <leader>p :bprevious<CR>


" -------- settings of loaded plugins ---------

" UltiSnips
" Default trigger is <tab>, but set it to c-j
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"
" place private snippet files inside .vim/UltiSnips/ 
" (if that folder is not there, Ultisnips may create this 
" folder OR another folder ~/UltiSnips/ with UltiSnipsEdit, 
" but that other one sometimes isn't referenced referenced later, 
" which is a shitty bug. Still, it always worked with .vim/UltiSnips/
" also, choosing other custom folder names, e.g. private_snippets and 
" pointing UltiSnips to them also didn't work

" Jedi
" let g:jedi#auto_initialization = 0
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "left"
" let g:jedi#popup_on_dot = 0
" let g:jedi#popup_select_first = 0
" let g:jedi#show_call_signatures = "1"

" NOTE: subject to change!

let g:jedi#goto_command = "<leader>d" 
" you can jump back to the original position using 
" vim's typical command C-o
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = ""
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>o"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"
let g:jedi#completions_enabled = 1

" let g:jedi#force_py_version = 3

" I don't want the docstring window to popup during completion
" autocmd FileType python setlocal completeopt-=preview

" Autopep8
let g:autopep8_max_line_length=79
let g:autopep8_indent_size=4

" add aggressive option
" let g:autopep8_aggressive=1
" add more aggressive options
" let g:autopep8_aggressive=2

" syntastic - STATUS: not used since it doesnt reference the current python
" virtualenv (also not with vim-conda) 
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

" let g:syntastic_python_checkers = ['pylint']
" instead, flake8 also checks pep8 and displays errors for that
" let g:syntastic_python_checkers = ['flake8']

let g:airline#extensions#tabline#enabled = 1
