export LANG=en_US.UTF-8
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
export PATH="$HOME/.nodenv/bin:$HOME/go/bin:$PATH"
setopt HIST_IGNORE_DUPS

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="yanne"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line
bindkey "\e[3~" delete-char

export PYTHONSTARTUP=$HOME/.pythonrc
export VIRTUAL_ENV_DISABLE_PROMPT="true"
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
source $HOME/.local/bin/virtualenvwrapper.sh

eval "$(nodenv init -)"

# modified commands
alias diff='colordiff'              # requires colordiff package
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias g=git
alias f=firefox
alias pyclean="find . -name '*.pyc' -delete"

#alias vim=nvim
export EDITOR=vim

_direnv_hook() {
  trap -- '' SIGINT;
  eval "$("/usr/bin/direnv" export zsh)";
  trap - SIGINT;
}
typeset -ag precmd_functions;
if [[ -z ${precmd_functions[(r)_direnv_hook]} ]]; then
  precmd_functions=( _direnv_hook ${precmd_functions[@]} )
fi
typeset -ag chpwd_functions;
if [[ -z ${chpwd_functions[(r)_direnv_hook]} ]]; then
  chpwd_functions=( _direnv_hook ${chpwd_functions[@]} )
fi

source <(kubectl completion zsh)
