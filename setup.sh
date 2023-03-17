#!/bin/bash
ENVO_DIR=${HOME}/source/chetanvaity/envo

# Dirs
mkdir -p ${HOME}/bin

# Setup a new work Mac
echo "Set up git..."
# Install TBD
rm -f ~/.gitconfig ~/.gitignore_global ~/bin/gcrb ~/bin/gp ~/bin/gpc ~/bin/gmu
ln -s ${ENVO_DIR}/git/.gitconfig ${HOME}/.gitconfig
ln -s ${ENVO_DIR}/git/.gitignore_global ${HOME}/.gitignore_global
ln -s ${ENVO_DIR}/git/gcrb ${HOME}/bin/gcrb
ln -s ${ENVO_DIR}/git/gp ${HOME}/bin/gp
ln -s ${ENVO_DIR}/git/gpc ${HOME}/bin/gpc
ln -s ${ENVO_DIR}/git/gmu ${HOME}/bin/gmu

echo "Setup bash..."
rm -f ~/.bash_aliases ~/.bashrc ~/.bash_profile
ln -s ${ENVO_DIR}/bash/.bash_aliases ${HOME}/.bash_aliases
ln -s ${ENVO_DIR}/bash/.bash_myrc ${HOME}/.bashrc
ln -s ${ENVO_DIR}/bash/.bash_profile ${HOME}/.bash_profile

echo "Setup ssh..."
rm -f ~/.ssh/config
ln -s ${ENVO_DIR}/ssh/config ~/.ssh/config

echo "Setup iterm2..."
echo "Manually go to Settings -> General -> Preferences -> Load Preferences from custom folder"
echo "Choose ${HOME}/source/chetanvaity/envo/iterm2"

echo "Setup k8s..."
rm -f ~/bin/kube-ps1.sh
ln -s ${ENVO_DIR}/k8s/kube-ps1.sh ${HOME}/bin/kube-ps1.sh
curl -L "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/darwin/amd64/kubectl" --output ${HOME}/bin/kubectl
chmod 755 ${HOME}/bin/kubectl

