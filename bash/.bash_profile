# Set a more linux like prompt
export PS1="[\\u@\\h \\W]$ "

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

# PATH modifications
export PATH=$PATH:$HOME/bin
#export DYLD_LIBRARY_PATH=/Library/PostgreSQL/9.3/lib
#export PATH=/usr/local/opt/ruby/bin:$PATH
#export PATH=$HOME/.gem/ruby/2.6.0/bin/:$PATH
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
#export PATH=$PATH:/Users/chetan/.linkerd2/bin
export PATH=/opt/homebrew/bin/:$PATH
export PATH=$PATH:$HOME/.tfenv/bin

# krew (kubectl plugins)
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
# To use GNU find, GNU sed
PATH=$(brew --prefix)/opt/findutils/libexec/gnubin:$PATH
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"
kubectx aws-tellus=arn:aws:eks:us-east-2:809541265033:cluster/tellus-multitenant
kubectx gcp-tellus=gke_stone-bounty-249217_us-central1-c_tellus-multitenant-ver2


# Lose no history
export HISTFILESIZE=1000000
export HISTSIZE=10000
export PROMPT_COMMAND="${PROMPT_COMMAND:-:} ; history -a"

shopt -s histappend

# Lets see if kube-ps1 works
source "$HOME/bin/kube-ps1.sh"
export PS1='$(kube_ps1)'$PS1

source <(kubectl completion bash)

# Created by `pipx` on 2022-07-12 20:55:20
export PATH="$PATH:/Users/chetan/.local/bin"

# gvm (Go versions)
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/chetan/bin/google-cloud-sdk/path.bash.inc' ]; then . '/Users/chetan/bin/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/chetan/bin/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/chetan/bin/google-cloud-sdk/completion.bash.inc'; fi

# Granica superman
if [ -f $GOPATH/src/project.n/sync_toolchain.py ]; then
    eval $($GOPATH/src/project.n/sync_toolchain.py --env)
else
    echo "WARNING: Granica environment script could not be located."
fi

# NVM (for projectn-docs)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


assume_profile() {
    local profile="$1"
    local caller_identity="$(aws sts get-caller-identity --profile "$profile" 2>/dev/null)"

    if [[ -n "$caller_identity" ]]; then
        export AWS_PROFILE="$profile"
        echo "Assumed profile: $profile"
        echo "Caller Identity:"
        echo "$caller_identity"
    else
        echo "Unable to assume profile '$profile', attempting AWS SSO login..."
        aws sso login --profile "$profile"
        local sso_login_status=$?

        if [[ $sso_login_status -eq 0 ]]; then
            echo "AWS SSO login successful. AWS_PROFILE is now set to '$profile'."
            export AWS_PROFILE="$profile"
        else
            echo "AWS SSO login failed. AWS_PROFILE remains unchanged."
        fi
    fi
}

