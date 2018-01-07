# Vim, get help to any key you press or command you enter
:help

# This will show you a list of all added (tracked) files
git ls-tree home-ubuntu1604 -r
# on rebase, when merge conflicts need to be resolved, after editing, 
# for whatever files you manually resolved do
git add [those files]
# then continue the rebase (without another merge-resolve-commit) using
git rebase --continue
# Vim, jumping back to previous position
c-o
# Vim, jumping to the next tab
gt
# Vim, jumping to the previous tab
gT
# Vim, open a list of .py files in tabs (-p) as opposed to whole windows
vim -p a.py b.py c.py
# Vim, yank 4 lines to linux system clipboard register (+)
V3j"+y
# Vim, paste system clipboard register into buffer (+)
"+p
# Vim, record a series of key presses on register 1 and play it 3 times
q1 ... q
3@1
# Vim, delete inside the sorrounding brackets )]}
di]
# Vim, yank inside bracket
yi]
# Vim, delete/yank all bracket
da]    ya]
# Vim, delete inside word 
diw / daw
# Vim, change inside/all word (diw but it puts you in insert mode afterwards)
ciw / caw
# Vim, delete/yank until (t) a character (space)
yt 
# Vim, visually select until/inside/all [character] (instead of y or d)
# General pattern for quick text manipulation:
[c/d/y/v][i/a/t/f][character]
[change/delete/yank/visually select][inside/all/til/forward til/][environment
sorrounded by character(s)]
# Vim, go to contents of registers (when you mess up/want to copy macros)
:reg
# Vim, get command history 
:history

# Vim
# ^ gets you to the first non-whitespace character in the line
# 0 gets you to the beginning of the line incl. whitespace


# Vim - time travel
# ":earlier {N,count}[s/m/h/d/f (file writes)]
# ":later {N,count}[s/m/h/d/f (file writes)]
# ":earlier 1f" will go to before the first change.
# ":later 1f" will go to the newest text state.

# Vim move to top (High) / Middle / bottom (Low) of the screen
H
M
L
# Vim, search case-insensitive, type
/\searchword


# bash Terminal, get the return value of last operation:
echo $?


# Tmux
# tmuxcheatsheet.com
# reload source file
:source-file ~/.tmux.conf   
# or
$ tmux source-file ~/.tmux.conf

# tmux, create window 
prefix c
# tmux, next window
prefix n
# tmux, previous window
prefix p
# tmux, window number N
prefix N
# tmux, move through panes (after remapping to hjkl)
prefix hjkl
# tmux, split vertically
prefix %
# tmux, split horizontally
prefix "
# tmux, toggle active pane between zoomed and unzoomed
prefix z
# tmux, detach
prefix d
# tmux, rename window
prefix ,
# tmux, start new session
$ tmux -u
# tmux, kill whole session
:kill-session
# tmux, kill the whole window (all panes)
prefix &
# tmux, kill active pane (type exit in the terminal is the preferred way)
prefix x
# tmux, get into copy mode using 
prefix [
# then, move around using arrow keys and select 
# by default using emacs select key bindings c+space
# and M(Alt)+w to copy the selection

# tmux, activate vi bindings in copy mode, but yank is Enter and v for visual
# selection is space
setw -g mode-keys vi

# Installation: Package managers vs. no Package managers
Package managers keep track of the files that come with a package (bins, 
sources, configs, ...). 
They keep a record of which files were installed (put) where, giving them the
ability to properly update, upgrade, remove, purge. They also keep track of
depenencies of a package on other packages. If no installed package points 
If a program is being compiled from source using 
```
$ ./configure
$ sudo make
$ sudo make install 
``` 
(use `./configure --prefix="~/opt/..."` (usually) to make `sudo make install`
put the binaries in a specified directory and not just dump them inside
`/usr/local/bin`). If they are installed just inside `/usr/local/bin`, then 
you may be able to remove those either going manually through the Makefile 
and removing installed files 1 by 1, or often there will be a `make uninstall` 
command available (before you `make uninstall`, run `./configure ...` with the same 
options that were provided to `./configure ...` when first doing `make install`.)

# Conda
## install alongside non-anaconda python installations
This is totally possible. Just put your anaconda bin path before (or after) the
system installation path (usually /usr/local/bin) in $PATH
## list virtual environments
```
conda info --env
```

## list installed python packages in env myenv
```
conda list -n myenv
```
## list installed python packages in active env
```
conda list
```

## search and install packages with conda (to conda)
```
conda search scipy=0.15.0
conda search curl
conda install scipy curl
```

### install a package for a specific python version (env name here py34_env):
```
conda install scipy=0.15.0 curl=7.26.0 -n py34_env
```

### search in specific anaconda channel (here conda-forge) for package (here ipdb)
```
conda search -c conda-forge ipdb
```

### install package from specific anaconda channel
```
conda install -c conda-forge ipdb
```

# Overview anaconda, conda, python, data science
## Anaconda
Anaconda is a Python Data Science Platform, meaning that when talking about
the software Anaconda, we talk about writing data science programs in python. 
Anaconda is also the name of the Organization that develops the package manager
and virtual environment manager conda.
This Platform comes in different forms for different people: Anaconda
Distribution (free, open source), and Anaconda Enterprise (where Anaconda is
embedded into a framework that makes large-scale collaboration with access rights, version control
and cloud computing easy), and is a commercial product.
### Anaconda Distribution
The Anaconda Distribution is a bundle of software (cross-platform) that 
1. ships it's own python installation (decoupled from previously installed python) 
and 2nd, ships the package manager conda (Windows, Linux, MacOS) (which has
been developed by the Anaconda Organization). They try to make it quick and easy 
to setup a python (and any other programming language (conda)) environment anywhere, 
without getting bogged down by OS-Specific details. When setting Anaconda up
on different systems, it's goal is to provide an environment for reproducible
data science and machine learning results accross different machines.
### Conda 
Conda is a program that creates and manages virtual environments where programs
of many different programming languages can run (isolated from the outside).
In a virtual environment, only one specific version of the programming language
(python) runs. The command conda is also a package manager. It installs packages 
in the anaconda3/bin directory. One can also install certain versions of certain
packages to be used by a specific venv with a specific python version.
