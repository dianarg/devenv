#!/bin/bash

cp -r scripts $HOME/
cp -r emacs_libs $HOME/
cp --backup=numbered .bashrc $HOME/
cp --backup=numbered .emacs $HOME/
cp --backup=numbered .gitconfig $HOME/
cp --backup=numbered .tmux.conf $HOME/
mkdir -p $HOME/.config
cp --backup=numbered pep8 $HOME/.config/pep8
