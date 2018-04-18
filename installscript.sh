# don't execute as sudo, because some settings are user-specific
echo "Update & Upgrade"
sudo apt-get update && sudo apt-get upgrade

echo "Installing git and vim"
# install crucial tools
sudo apt install -y git git-annex vim
# install vim plugins
# Vundle 
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
# install plugins 
vim -c PluginInstall

echo "done"
read -n1 -r -p "Press any key to continue... (installing suite of helper tools)" key

sudo apt install -y xclip xdg-utils htop openssh-client curl tmux zsh samba samba-common smbclient build-essential python3-pip tree
sudo pip3 install scipy matplotlib numba flake8 ipdb sympy python3-tk


# dotfiles
# git clone https://github.com/534ttl3/dotfiles
cd ~
git init 
git add remote origin git@github.com/534ttl3/dotfiles.git
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

# oh-my-zsh
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
chsh -s $(which zsh)  # this shouldn't be executed as sudo

# custom stuff
echo "---- custom stuff begin ----"
read -n1 -r -p "Press any key to continue..." key

echo "adding ssh key with ssh-keygen ..."
ssh-keygen

echo "--> if you wish, register your public key with github at "
echo "    https://github.com/settings/keys"
echo "--> and then pull down your dotfiles into your ~ directory"

echo "--> please now reboot the system so that all changes take effect"
