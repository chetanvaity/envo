#!/bin/bash
ENVO_DIR=${HOME}/sourc/chetanvaity/envo

# Dirs
mkdir -p ${HOME}/bin

# Setup a new work Mac
echo "Set up git..."
# Install TBD
ln -s ${ENVO_DIR}/git/.gitconfig ${HOME}/.gitconfig
ln -s ${ENVO_DIR}/git/.gitignore_global ${HOME}/.gitignore_global
ln -s ${ENVO_DIR}/git/gcrb ${HOME}/bin/gcrb
ln -s ${ENVO_DIR}/git/gp ${HOME}/bin/gp
ln -s ${ENVO_DIR}/git/gpc ${HOME}/bin/gpc
ln -s ${ENVO_DIR}/git/gmu ${HOME}/bin/gmu

echo "Setup bash..."
ln -s ${ENVO_DIR}/bash/.bash_aliases ${HOME}/.bash_aliases
ln -s ${ENVO_DIR}/bash/.bash_myrc ${HOME}/.bashrc
ln -s ${ENVO_DIR}/bash/.bash_profile ${HOME}/.bash_profile

echo "Setup k8s..."
ln -s ${ENVO_DIR}/k8s/kube-ps1.sh ${HOME}/bin/kube-ps1.sh
curl -L "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/darwin/amd64/kubectl" --output ${HOME}/bin/kubectl
chmod 755 ${HOME}/bin/kubectl
