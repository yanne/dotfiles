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
plugins=(git)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line
bindkey "\e[3~" delete-char

export EDITOR=vim
export JAVA_HOME=/opt/java
export JDK_HOME=/opt/java
export JYTHON_HOME=/opt/jython
export PYTHONSTARTUP=$HOME/.pythonrc

export VIRTUAL_ENV_DISABLE_PROMPT="true"
export WORKON_HOME=$HOME/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

PATH=$PATH:$HOME/.cabal/bin

function noksu() {
	svnfile=$HOME/.subversion/servers
    mvnfile=$HOME/.m2/settings.xml
    hgfile=$HOME/.hgrc
    spotify=$HOME/.config/spotify/settings
	proxy_host=10.144.1.10
	if [[ "$1" == off ]]; then
		echo "noksu mode off"
		export http_proxy=
        export https_proxy=
        sed -i "s/host=/#host=/" $hgfile
		sed -i "s/http-proxy-host = $proxy_host/# http-proxy-host =/" $svnfile
		sed -i "s/http-proxy-port = 8080/# http-proxy-port =/" $svnfile
        sed -i "s|<proxy>|<!--proxy>|" $mvnfile
        sed -i "s|</proxy>|</proxy-->|" $mvnfile
        sed -i 's/"proxy_mode":2/"proxy_mode":1/' $spotify
	else
		echo "noksu mode on"
		export http_proxy="http://$proxy_host:8080"
		export https_proxy=$http_proxy
        sed -i "s/#host=/host=/" $hgfile
		sed -i "s/# http-proxy-host =/http-proxy-host = $proxy_host/" $svnfile
		sed -i "s/# http-proxy-port =/http-proxy-port = 8080/" $svnfile
        sed -i 's|<!--proxy>|<proxy>|' $mvnfile
        sed -i 's|</proxy-->|</proxy>|' $mvnfile
        sed -i 's/"proxy_mode":1/"proxy_mode":2/' $spotify
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
alias p=python2
alias n=nosetests
alias pyclean="find . -name '*.pyc' -delete -or -name '*$py.class' -delete"

# setting up work environs
alias ride='workon robot-dev && cd ~/work/RIDE'
alias robot='workon robot-dev && cd ~/work/robotframework'
