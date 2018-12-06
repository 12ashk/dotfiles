export LANG='ja_JP.UTF-8'
## 重複パスを登録しない
typeset -U path cdpath fpath manpath

## sudo用のpathを設定
typeset -xT SUDO_PATH sudo_path
typeset -U sudo_path
sudo_path=({/usr/local,/usr,}/sbin(N-/))

## pathを設定
path=(~/bin(N-/) /usr/local/bin(N-/) ${path})

autoload -U compinit && compinit
setopt auto_list
setopt auto_menu
setopt correct

autoload colors; colors
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:*' formats '(%s)[%b]'
zstyle ':vcs_info:*' actionformats '(%s)[%b|%a]'
#PROMPT="%{${fg[yellow]}%}%~%{${reset_color}%}${vcs_info_msg_0_}
#[%n]$ "
precmd() { vcs_info }  
RPROMPT='${vcs_info_msg_0_}'
PROMPT='%F{yellow}%d%f
[%n]$ '

setopt auto_cd
setopt autopushd
setopt pushd_ignore_dups
 
disable r

# alias
alias ls='ls -AF'
alias ll='ls -lF'
alias la='ls -lAF'
alias df="df -h"
alias du="du -h"
alias gvim='open -a MacVim'
alias lock='/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine'
alias less='/usr/share/vim/vim73/macros/less.sh'
#alias sshoka='ssh okalab@192.168.11.54'
alias sshoka='ssh okalab@192.168.11.49'
 
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
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt extended_history
setopt share_history

# Homebrew
export PATH=/usr/local/bin:/usr/local/sbin:/usr/local/share:/Library/local/texbin:$PATH
typeset -U path cdpath fpath manpath
typeset -xT SUDO_PATH sudo_path
typeset -U sudo_path
sudo_path=({/usr/local,/usr,}/sbin(N-/))

#export CC=/usr/bin/gcc
export GNUTERM=Plinix11

#export PYTHONPATH=:$(brew --prefix)/lib/python2.7/site-packages:$PYTHONPATH
export LIBRARY_PATH=$LIBRARY_PATH:/usr/local/lib
export C_INCLUDE_PATH=/usr/local/include:/opt/X11/include:/opt/gtk-x11/include
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/local/include:/usr/X11/include:/usr/gtk-x11/include:/usr/local/Cellar/eigen/3.2.1/include/eigen3
export CPATH=$CPATH:/usr/local/bin

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/bin:$PATH"
eval "$(rbenv init -)"
