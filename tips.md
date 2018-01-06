# Vim, get help to any key you press or command you enter
:help

# This will show you a list of all added (tracked) files
git ls-tree home-ubuntu1604 -r

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

