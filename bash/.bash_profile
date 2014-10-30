# Set a more linux like prompt
#export PS1="[\\u@\\h \\W]$ "

# prompt with GIT branch
function color_my_prompt {
    local __user_and_host="\[\033[01;32m\]\u@\h"
    local __cur_location="\[\033[01;34m\]\w"
    local __git_branch_color="\[\033[01;31m\]"
    #local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'
    local __git_branch='`git branch 2> /dev/null | grep --color=never -e ^* | sed s/..//`'
    local __prompt_tail="\[\033[35m\]$"
    local __last_color="\[\033[00m\]"
    export PS1="$__user_and_host $__cur_location$__git_branch_color $__git_branch $__prompt_tail$__last_color "
}
color_my_prompt

# Get some colours
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# All aliases
source $HOME/.bash_aliases

# Java related
export JAVA_HOME=$(java_home)