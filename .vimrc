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
  
" Jedi
Plugin 'davidhalter/jedi-vim'

" Nerdtree
Plugin 'scrooloose/nerdtree'
" Nerdtree execute file in gnome standard program
Plugin 'ivalkeen/nerdtree-execute'

" Syntastic (syntax checking) for python
Plugin 'vim-syntastic/syntastic'

" vim-airline (beautiful status line)  
" # WARNING: Do 24. Mai 11:03:50 CEST 2018: 
" vim-airline may cause delays when switching buffers
Plugin 'vim-airline/vim-airline'
" Plugin 'vim-airline/vim-airline-themes'

" tmuxline, enables vim-airline to work within tmux
Plugin 'edkolev/tmuxline.vim'

" CtrlP
Plugin 'kien/ctrlp.vim'

" vimux, call tmux command from inside vim and run in it in seperate pane
Plugin 'benmills/vimux'

" autopep8
Plugin 'tell-k/vim-autopep8'

" simple latex folding
Plugin 'matze/vim-tex-fold'

" replace only visually selected region, not the whole line
Plugin 'vim-scripts/vis'

" Latex-Suite
" Plugin 'vim-latex/vim-latex'
" let g:Tex_AutoFolding = 0


" vim-conda - STATUS: currently not used, sice my worklflow is to activate 
" a virtualenv in a seperate shell and I don't use any code-completion or 
" linting tools that can reference the python packages in that virtualenv
" enables to switch the python version that vim uses
" within it's running process (not :!python)
" Plugin 'cjrh/vim-conda'

call vundle#end()            " required

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ

" ------ other non-Vundle plugins ------
" Python folding
" another good one: http://www.vim.org/scripts/script.php?script_id=2527
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492

" or just without plugin: 
" :setlocal foldmethod=indent

" or with this plugin

" ------ general vim settings (not plugin-related) ------

" enable ftplugin's automatic indenting
filetype plugin indent on

" Real programmers don't use TABs but spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab

" use system clipboard to yank and paste
set clipboard=unnamed 

" Rebind <Leader> key
let mapleader = ","

" Nerdtree rebind
nmap <leader>t :NERDTreeToggle<cr>

" be able to leave buffer without saving (hide buffer), but still 
" warn at leaving if some buffer isn't saved
set hidden

syntax on

" disable backup and swap files - they may trigger many events
" for file system watchers
" set nobackup
" set nowritebackup
" set noswapfile
" or back them up in a seperate folder
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

set encoding=utf-8

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" make backspace key work properly in insert mode
set backspace=indent,eol,start

" colorscheme default
colorscheme darkblue

" Showing line numbers and length
set number  " show line numbers
set tw=79   " width of document (used by gd)
" set nowrap  " don't automatically wrap on load
set wrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
" draw a vertical column (to indicate long lines)
set colorcolumn=80
" highlight ColorColumn ctermbg=233

augroup PythonDisplayAndMoveSettings
    autocmd!
    let b:folded = 1

    function! ToggleFold()
        if( b:folded == 0 )
            exec "normal! zM"
            let b:folded = 1
        else
            exec "normal! zR"
            let b:folded = 0
        endif
    endfunction
    autocmd FileType python setlocal scrolloff=3
    autocmd FileType python map <buffer> f za
    autocmd FileType python map <buffer> F :call ToggleFold()<CR>
    " Vimux
    autocmd FileType python nmap <leader>z :call VimuxRunCommand("python3 main.py")<cr>
    autocmd FileType python nmap <leader>% :call VimuxRunCommand("python3 " + %)<cr>
augroup END


augroup LaTeXDisplayAndMoveSettings
    autocmd!
    autocmd FileType tex setlocal wrap
    autocmd FileType tex setlocal colorcolumn=0
    autocmd FileType tex setlocal linebreak
    autocmd FileType tex nnoremap <buffer> j gj
    autocmd FileType tex nnoremap <buffer> k gk
    " Vimux
    autocmd FileType tex nmap <leader>la :call VimuxRunCommand("latexmain")<cr>
    autocmd FileType tex setlocal scrolloff=5

augroup m3uEditingSettings
    autocmd!
    autocmd FileType m3u nmap <Leader>a /=lC1Vv:B !awk '{ split($0,a,":"); print (a[1]*60)+a[2] }' <<< "
augroup END

" nmap <Leader>a /=lC1Vv:B !awk '{ split($0,a,":"); print (a[1]*60)+a[2] }' <<< "

" navigate splits more efficiently
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" quickly cycle through buffers
:nnoremap <leader>n :bnext<CR>
:nnoremap <leader>p :bprevious<CR>

" vimgrep, cylce through quickfix list
nnoremap [q :cprev<CR>
nnoremap ]q :cnext<CR>
nnoremap [Q :cfirst<CR>
nnoremap ]Q :clast<CR>


map <leader>b Oimport ipdb; ipdb.set_trace()  # noqa BREAKPOINT<C-c>
" -------- settings of loaded plugins ---------


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" set ft plaintex to latex, so that UltiSnips doesn't create the
" plaintex.snippets file for newly created (empty) tex files
" you can check the filetype by typing :set filetype
let g:tex_flavor="latex"


" Jedi
" let g:jedi#auto_initialization = 0
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#use_tabs_not_buffers = 1
let g:jedi#use_splits_not_buffers = "left"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#show_call_signatures = "1"

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

let g:jedi#force_py_version = 3

let g:jedi#call_signature_escape="'=`='"


" I don't want the docstring window to popup during completion
" autocmd FileType python setlocal completeopt-=preview

" Autopep8
let g:autopep8_max_line_length=79
let g:autopep8_indent_size=4

" syntastic - STATUS: don't use this if you want to handle different
" virtualenvs, that doesn't work
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

let g:syntastic_python_checkers = ['flake8']
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': ['python', 'json'],'passive_filetypes': [] }

" airline
let g:airline#extensions#tabline#enabled = 1
" let g:airline_powerline_fonts = 1
" install powerline fonts:
" https://powerline.readthedocs.io/en/master/installation/linux.html#installation-on-linux
