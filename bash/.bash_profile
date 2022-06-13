# Set a more linux like prompt
#export PS1="[\\u@\\h \\W]$ "

# prompt with GIT branch
function color_my_prompt {
    #local __user_and_host="\[\033[01;32m\]\u@\h"
    local __user_and_host=""
    local __cur_location="\[\033[01;34m\]\w"
    local __git_branch_color="\[\033[01;31m\]"
    #local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'
    local __git_branch='`git branch 2> /dev/null | grep --color=never -e ^* | sed s/..// | sed s/^/\(/ | sed s/$/\)/`'
    local __prompt_tail="\n\[\033[35m\]$ "
    local __last_color="\[\033[00m\]"
    export PROMPT_DIRTRIM=2
    export PS1="$__user_and_host $__cur_location$__git_branch_color $__git_branch $__prompt_tail$__last_color"
}
color_my_prompt

# Get some colours
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# Ignore some files in bash tab-completion
export FIGNORE='.git:~'

# All aliases
source $HOME/.bash_aliases

# Java related
#export JAVA_HOME=$(java_home)
# The next line updates PATH for the Google Cloud SDK.
#source '/Users/chetanv/google-cloud-sdk/path.bash.inc'

# The next line enables bash completion for gcloud.
#source '/Users/chetanv/google-cloud-sdk/completion.bash.inc'

export PATH=$PATH:$HOME/bin
#export DYLD_LIBRARY_PATH=/Library/PostgreSQL/9.3/lib
export PATH=/usr/local/opt/ruby/bin:$PATH
export PATH=$HOME/.gem/ruby/2.6.0/bin/:$PATH
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH=$PATH:/Users/chetan/.linkerd2/bin

# krew (kubectl plugins)
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
# To use GNU find, GNU sed
PATH=$(brew --prefix)/opt/findutils/libexec/gnubin:$PATH
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"

# Lose no history
export HISTFILESIZE=1000000
export HISTSIZE=10000
export PROMPT_COMMAND="${PROMPT_COMMAND:-:} ; history -a"

shopt -s histappend

# Lets see if kube-ps1 works
source "$HOME/bin/kube-ps1.sh"
export PS1='$(kube_ps1)'$PS1

# Orbital
export PYTHONPATH=$OI_TOOL/libs/python-common

source <(kubectl completion bash)
