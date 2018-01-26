# install a few assistant tools
```
sudo apt-get install xclip
sudo apt-get install xdg-utils
sudo apt-get install libgnome2  # gnome-open
sudo apt-get install vim
sudo apt-get install htop
sudo apt-get install openssh-client
sudo apt-get install openssh-server
sudo apt-get install inkscape
sudo apt-get install curl
sudo apt-get install zsh
sudo apt-get install ffmpeg
sudo apt-get install tree
```
# install SSH and add keys to remote repos
```
ssh-keygen
```
don't overwrite the existing rsa key pair
go to github and add the contents of of your public key file
```
cat $HOME/.ssh/id_rsa.pub
```

# install dotfiles repo from github
you probably have already installed git, so init a git repo in $HOME
```
cd $HOME
git init
git remote add origin git@github.com:534ttl3/dotfiles.git
git remote -v
git config --global user.email "you@example.com"
git config --global user.name "Your Name"
git pull
git checkout -b newmachine-osversion
git checkout -b oldmachineconfigtobaseon-osversion
```

this gets you a list of already-existing files, add them to the new commit 
then merge a remote branch (a similar system) into 
the already existing branch (new files) and resolve conflicts

```
git branch --set-upstream-to=origin/<branch> master
```

# C/C++
install gnu compilers
```
sudo apt-get install build-essentials
```

# Python
```
sudo apt-get install pip
```
Install latest python version (anaconda) into `$HOME/opt/`

## install scipy, matplotlib, numba, ...
always install to the right locations (`pip` could be `pip2`, not `pip3`)
```
sudo pip3 install scipy matplotlib numba flake8 ipdb
```
matplotlib could use tkinter, so do
```
sudo apt install python3-tk
```

# other TODO's for setting up a new system

## vim 
```
sudo apt-get install vim
```
and compile vim with `+conceal` and `--prefix=/opt/` (in `--with-feature=huge`) (jedi parameter list completion) `+python` and `+python3` 
support and add custom bin path to $PATH in .bashrc
on `https://github.com/Valloric/YouCompleteMe/wiki/Building-Vim-from-source`,
there is a good tutorial on how to compile vim with certain options.

## vundle plugin manager
```
git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
vim -c PluginInstall
```

## non-vundle-managed plugins
### Python folding
```
mkdir -p ~/.vim/ftplugin
```
I prefer
```
wget -O ~/.vim/ftplugin/python.vim http://www.vim.org/scripts/script.php?script_id=2527
``` 
But this one is also good
```
wget -O ~/.vim/ftplugin/python.vim http://www.vim.org/scripts/download_script.php?src_id=5492
```

# install powerline fonts (for vim-airline)
follow the instructions at 
```
https://powerline.readthedocs.io/en/master/installation/linux.html#installation-on-linux
```

### plugins dependencies
```
pip install jedi
pip install autopep8 pep8
```

## tmux
```
sudo apt-get install tmux
```
## latex and pdf viewer
```
sudo apt-get install texlive-full
sudo apt-get install latexmk
sudo apt-get install zathura
```

## zsh
Go to ohmyzsh.sh or just run
```
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
```
