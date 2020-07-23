#!/bin/bash

cp -r scripts $HOME/
cp -r emacs_libs $HOME/
cp --backup=numbered .bashrc $HOME/
cp --backup=numbered .emacs $HOME/
cp --backup=numbered .gitconfig $HOME/
cp --backup=numbered .tmux.conf $HOME/
mkdir -p $HOME/.config
cp --backup=numbered pep8 $HOME/.config/pep8

read -p "Install commandline fuzzy finder? [Yy/Nn]" yn
case $yn in
    [Yy]* ) git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install; break;;
    * ) exit;;
esac
