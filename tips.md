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
