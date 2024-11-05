alias emacs='/usr/local/bin/emacs' 

alias grep='grep --color=always --exclude-dir=".git"'
alias less='less -R'
#alias ssh="ssh -v -4"
alias l="ls --color"
alias ll="ls -al --color"
alias la="ls -al --color"
alias prettyjson='python -m json.tool'
alias dig="dig +short"

# Git
alias g='git'
alias gd="git diff"
alias gs="git status"
alias gb="git branch --sort=-committerdate"

# Orbital
#alias cdo="cd ~/source/orbitalinsight"

# Project N
alias cdp="cd ~/source/projectn"
alias cdo=cdp

# Kubernetes
alias k="kubectl"
alias kcu="k config use"
alias kgp="k get pod -o wide"
alias kl="k logs"
alias kdp="k describe pod"
alias kdn="k describe node"
kn() { kubectl config set-context --current --namespace="$1"; }

# Emacs
alias eo="emacs ~/source/orbitalinsight/ &"
alias eh="emacs . &"
