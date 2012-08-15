export LANG=en_US.UTF-8
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

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
plugins=(git mercurial)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line
bindkey "\e[3~" delete-char

export EDITOR=vim
export JAVA_HOME=$(/usr/libexec/java_home)
export JDK_HOME=$JAVA_HOME
export JYTHON_HOME=/usr/local/Cellar/jython/2.5.2/libexec
export PYTHONSTARTUP=$HOME/.pythonrc

export VIRTUAL_ENV_DISABLE_PROMPT="true"
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

PATH=$PATH:$HOME/.cabal/bin

function proxy() {
	svnfile=$HOME/.subversion/servers
    mvnfile=$HOME/.m2/settings.xml
	proxy_host=10.144.1.10
    proxy_port=8080
	if [[ "$1" == off ]]; then
		echo "proxy mode off"
		unset http_proxy
        unset https_proxy
        unset all_proxy
		sed -i "s/http-proxy-host = $proxy_host/# http-proxy-host =/" $svnfile
		sed -i "s/http-proxy-port = 8080/# http-proxy-port =/" $svnfile
        sed -i "s|<proxy>|<!--proxy>|" $mvnfile
        sed -i "s|</proxy>|</proxy-->|" $mvnfile
	else
		echo "proxy mode on"
		export http_proxy="http://$proxy_host:proxy_port"
		export https_proxy=$http_proxy
        export all_proxy=$http_proxy
		sed -i "s/# http-proxy-host =/http-proxy-host = $proxy_host/" $svnfile
		sed -i "s/# http-proxy-port =/http-proxy-port = 8080/" $svnfile
        sed -i 's|<!--proxy>|<proxy>|' $mvnfile
        sed -i 's|</proxy-->|</proxy>|' $mvnfile
	fi
}

# modified commands
alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias prsudo='sudo env http_proxy=http://10.144.1.10:8080'
alias g=git
alias v=vim
alias f=firefox
alias p=python2.7
alias n=nosetests
alias pyclean="find . -name '*.pyc' -delete"

# setting up work environs
alias ride='workon robot-dev && cd ~/code/RIDE'
alias robot='workon robot-dev && cd ~/code/robotframework'
