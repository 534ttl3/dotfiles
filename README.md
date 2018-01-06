# --- install a few assistant tools ---
sudo apt-get install xclip
sudo apt-get install vim
sudo apt-get install htop
sudo apt-get install openssh-client

# --- install SSH and add keys to remote repos ---
ssh-keygen
# don't overwrite the existing rsa key pair
# go to github and add the contents of of your public key file
cat $HOME/.ssh/id_rsa.pub

# --- install dotfiles repo from github ---
# you probably have already installed git, so init a git repo in $HOME
cd $HOME
git init
git remote add origin git://github.com/534ttl3/dotfiles.git
git remote -v
git pull
# this gets you a list of already-existing files, add them to a new branch
then merge a remote branch (a similar system) into 
# the already existing branch (new files) and resolve conflicts
# git branch --set-upstream-to=origin/<branch> master

# --- C/C++ ---
# install gnu compilers
sudo apt-get install build-essentials

# --- Python ---
sudo apt-get install pip
# Install latest python version (anaconda) into $HOME/opt/

# dotfiles Git repo
# other TODO's for setting up a new system

# --- vim ---
sudo apt-get install vim
# compile vim with +conceal (jedi parameter list completion), 
# +python and +python3 support

# vundle plugin manager
git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
vim -c PluginInstall

# non-plugin plugins
" Python folding
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492

# plugins dependencies
pip install jedi
pip install autopep8

# --- tmux ---
sudo apt-get install tmux

# --- latex ---
sudo apt-get install latexmk
sudo apt-get install texlive-full
sudo apt-get install zathura
