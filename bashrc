#/bin/bash #-*- sh -*-

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]]; then
    export PATH=$HOME/usr/bin:$PATH
    # Shell is non-interactive.  Be done now
    return
fi

if [[ -e /etc/bashrc ]]; then
    . /etc/bashrc
fi

source $HOME/.site-config
source $DOTC_DIR/color-bash

pathmunge () {
        if ! echo $PATH | /bin/egrep -q "(^|:)$1($|:)" ; then
           if [ "$2" = "after" ] ; then
              PATH=$PATH:$1
           else
              PATH=$1:$PATH
           fi
        fi
}

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

alias grep='grep --color=auto'
alias remacs='`grep emacs ~/.screenrc`'
#alias grephist='grep

# tailoring 'less'
alias more='less'
export PAGER=less
export LESSCHARSET='utf-8'
#export LESSCHARSET='latin1'

#export LESS='-i -M -F -R -w'
if [ -f /usr/bin/src-hilite-lesspipe.sh ]; then
	export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
	export LESS=' -R '
elif [ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
	export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
	export LESS=' -R '
else
	export LESS=' -M -R'          # Less stuff 
fi
#LESSEDIT="%E ?lt+%lt. %f" 

export HISTFILE=$HOME/.bashist/`date +%Y-%m-%d_%R`.$$.`hostname`
touch $HISTFILE && chmod 600 $HISTFILE

export PROMPT_COMMAND='history -w' #write to history whenever the prompt is displayed
export HISTSIZE=1000 # Long history
export HISTFILESIZE=10000 # Size of historyfile
export HISTCONTROL="ignoreboth" # Ignore duplicates
export HISTIGNORE="&:ls:ll:dir:la:[bf]g:exit"
set HISTTIMEFORMAT 

alias vi=vim

pathmunge ${HOME}/usr/bin

export CLASSPATH=.
export CPPFLAGS=-I$HOME/usr/include
export CFLAGS=$CPPFLAGS
if [[ -z $LD_LIBRARY_PATH ]]; then
	export LD_LIBRARY_PATH=$HOME/usr/lib
else
	export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/usr/lib
fi	
export LDFLAGS=-L$HOME/usr/lib

export CVS_RSH=ssh
#export CVSROOT=:ext:cc1@bedlam.cse.wustl.edu:/export/cvs

if [ -f $DOTC_DIR/bashrc-$DOTC_NAME ]; then
	source $DOTC_DIR/bashrc-$DOTC_NAME
fi
case $DOTC_NAME in 
    gentoo ) 
	;;
    cec )
	;;
    dreamhost )
    	;;
    bio )
	;;
esac

export HOSTFILE=$DOTC_DIR/hosts

export OUTPUT_OPTION=""
#export EDITOR=emacsclient
export EDITOR=vim

set -o emacs
shopt -s extglob cdspell checkwinsize cmdhist histverify hostcomplete >& /dev/null

bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ "$PS1" ] && [ $bmajor -eq 2 ] && [ $bminor '>' 04 ]; then  
    if [[ -f $DOTC_DIR/bash_completion/bash_completion ]]; then # interactive shell
	export BASH_COMPLETION=$DOTC_DIR/bash_completion
	. $DOTC_DIR/bash_completion/bash_completion
    elif [[ -f /etc/bash_completion ]]; then
	. /etc/bash_completion
    fi 
fi
unset bash bmajor bminor

if [[ `uname` != "SunOS" ]]; then
    if which keychain > /dev/null 2>&1; then
	keychain --ignore-missing -Q -q id_rsa id_dsa
	source ~/.keychain/$HOSTNAME-sh
    fi
fi

# gets called every time need to fix for once it's set
#if [[! -f ~/.xmodmap ]]; then
#    xmodmap $DOTC_DIR/xmodmap 
#fi

unset pathmunge
export PATH
xset b off > /dev/null 2>&1
