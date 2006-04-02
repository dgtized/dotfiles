#/bin/bash #-*- sh -*-

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]]; then
    # Shell is non-interactive.  Be done now
    return
fi

source $HOME/.home-config/site-config
#CONFIG_DIR="$HOME/.home-config"

#using bash
#set prompt='%{\033[1;34m%}[%! %n@%m %c]$%{\033[0;0m%} '
        
#setenv RUBYLIB ${HOME}/usr/lib:${HOME}/usr/lib/ruby:${HOME}/bin/ruby-1.8.0/ext/socket

source $CONFIG_DIR/color-bash

#PS1='\[\033[01;32m\]\u@\h \[\033[01;34m\]\w \$ \[\033[00m\]'
PS1="\[$red\][\! \u@\h \w]$\[$NC\] "

#-------------------
# Personnal Aliases
#-------------------

# -> Prevents accidentally clobbering files.
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i' 
alias mkdir='mkdir -p'

alias h='history'
alias j='jobs -l'
#alias which='type -all'
alias ..='cd ..'
alias path='echo -e ${PATH//:/\\n}'
#alias print='/usr/bin/lp -o nobanner -d $LPDEST'   # Assumes LPDEST is defined
#alias pjet='enscript -h -G -fCourier9 -d $LPDEST'  # Pretty-print using enscript
#alias background='xv -root -quit -max -rmode 5'    # Put a picture in the background
alias du='du -kh'
alias df='df -kTh'
alias quota='quota -s'
alias a2ps='a2ps -M Letter '

# The 'ls' family (this assumes you use the GNU ls)
alias ll='ls -l'               # show hidden files
alias la='ls -Al'               # show hidden files
alias ls='ls -hF --color=auto' # add colors for filetype recognition
alias lx='ls -lXB'              # sort by extension
alias lk='ls -lSr'              # sort by size
alias lc='ls -lcr' # sort by change time  
alias lu='ls -lur' # sort by access time   
alias lr='ls -lR'               # recursive ls
alias lt='ls -ltr'              # sort by date
alias lm='ls -al |more'         # pipe through 'more'
alias tree='tree -Csu' # nice alternative to 'ls'

# tailoring 'less'
alias more='less'
export PAGER=less
export LESSCHARSET='utf-8'
#export LESSCHARSET='latin1'

#export LESS='-i -M -F -R -w'
if [ -f /usr/bin/src-hilite-lesspipe.sh ]; then
	export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
	export LESS=' -R '
else
	export LESS=' -M '          # Less stuff 
fi
#LESSEDIT="%E ?lt+%lt. %f" 
#LESSCHARDEF="8bcccbcc13b.4b95.33b."

set HISTSIZE=1000 # Long history
set HISTFILESIZE=500 # Size of historyfile
set HISTCONTROL="ignoredups:erasedups" # Ignore duplicates
set HISTIGNORE='&:[bf]g:exit'# Ignore stuff

alias requiem='rdesktop -a16 -u comstocl -f 128.252.48.55 &'
alias oasis='rdesktop -a16 -u cc1 -f oasis.cec.wustl.edu &'
alias vi=vim

#setenv ACE_ROOT /home/cec/class/cs342/ACE_wrappers
#setenv LD_LIBRARY_PATH ${ACE_ROOT}/ace:/home/cec/class/cs342/lib
export CLASSPATH=.
export CPPFLAGS=-I$HOME/usr/include
export LD_LIBRARY_PATH=$HOME/usr/lib    
export LDFLAGS=-L$HOME/usr/lib
export CFLAGS=$CPPFLAGS

source $CONFIG_DIR/bashrc-$CONFIG_NAME

case $CONFIG_NAME in 
    gentoo ) 
	export PATH=${HOME}/usr/bin:${PATH}:/usr/sbin:/sbin
	alias eth0='sudo /etc/init.d/net.eth0'
	alias eth1='sudo /etc/init.d/net.eth1'
	alias shutdown='sudo /sbin/shutdown'
	alias iwconfig='sudo /sbin/iwconfig eth1'
	alias esync='sudo esync'
	alias emerge='sudo emerge'
	alias synjeff='ssh -f -N -L 24800:jeff.arl.wustl.edu:24800 comstocl@jeff.arl.wustl.edu && synergyc localhost'
	;;
    cec )
	;;
esac

export HOSTFILE=$CONFIG_DIR/hosts

export OUTPUT_OPTION=""
#export EDITOR=emacsclient
export EDITOR=vim

export CVS_RSH=ssh
export CVSROOT=:ext:cc1@bedlam.cse.wustl.edu:/export/cvs

shopt -s extglob cdspell checkwinsize cmdhist histverify hostcomplete >& /dev/null

bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ "$PS1" ] && [ $bmajor -eq 2 ] && [ $bminor '>' 04 ]; then  
    if [ -f $CONFIG_DIR/bash_completion/bash_completion ]; then # interactive shell
	export BASH_COMPLETION=$CONFIG_DIR/bash_completion
	. $CONFIG_DIR/bash_completion/bash_completion
    elif [ -f /etc/bash_completion]; then
	. /etc/bash_completion
    fi 
fi
unset bash bmajor bminor

if [[ `uname` != "SunOS" ]]; then
    if which keychain > /dev/null 2>&1; then
	keychain id_rsa id_dsa
	source ~/.keychain/$HOSTNAME-sh
    fi
fi

# gets called every time need to fix for once it's set
#if [[! -f ~/.xmodmap ]]; then
#    xmodmap $CONFIG_DIR/xmodmap 
#fi

xset b off > /dev/null 2>&1
