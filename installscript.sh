# don't execute as sudo, because some settings are user-specific

# install crucial tools
sudo apt install -y git git-annex vim
sudo apt install -y xclip xdg-utils htop openssh-client curl tmux zsh samba samba-common smbclient build-essential python3-pip tree
sudo pip3 install scipy matplotlib numba flake8 ipdb sympy python3-tk

# dotfiles
mkdir ~/projects
cd ~/projects
# sudo git clone ssh://git@github.com/534ttl3/dotfiles.git
git clone https://github.com/534ttl3/dotfiles

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


echo "--> please now reboot the system so that all changes take effect"
