export LANG=ja_JP.UTF-8

autoload -U compinit && compinit
setopt auto_list
setopt auto_menu
setopt correct

setopt prompt_subst
autoload colors
colors
PROMPT="%{${fg[yellow]}%}%~%{${reset_color}%}
[%n]$ "

setopt auto_cd
setopt autopushd
setopt pushd_ignore_dups
 
# alias
alias ls='ls -AF'
alias ll='ls -lF'
alias la='ls -lAF'
alias df="df -h"
alias du="du -h"
 
# not distinguish between lower case and upper case
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1

# options
setopt BASH_AUTO_LIST
setopt LIST_AMBIGUOUS
setopt AUTO_PUSHD

# history
HISTFILE="$HOME/.zsh_history"
HISTSIZE=1000000
SAVEHIST=1000000
#setopt hist_ignore_all_dups
#setopt hist_reduce_blanks
setopt extended_history
setopt share_history

# prioritize homebrew
export PATH=~/bin:/usr/local/bin:$PATH
alias gvim='open -a MacVim'

# MacPorts
export PATH=/opt/local/bin:/opt/local/sbin/:$PATH
export MANPATH=/opt/local/man:$MANPATH
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# Homebrew
export PATH=/usr/local/bin:/usr/local/sbin:/usr/local/share:/usr/local/share/python:$PATH 
alias lock='/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine'

export PYTHONPATH=$HOME/.pythonbrew/current:$(brew --prefix)/lib/python2.7/site-packages:$PYTHONPATH
export WORKON_HOME=$HOME/.virtualenvs
export GNUTERM=x11

export PATH=$PATH:$HOME/local/bin
export LIBRARY_PATH=$LIBRARY_PATH:$HOME/local/bin
export C_INCLUDE_PATH=$HOME/local/include
export CPLUS_INCLUDE_PATH=$HOME/local/include
export CPATH=$CPATH:/opt/local/include
