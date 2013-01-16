## This file is sourced by all *interactive* bash shells on startup.  This
## file *should generate no output* or it will break the scp and rcp commands.
############################################################

if [ -e /etc/bashrc ] ; then
  . /etc/bashrc
fi

############################################################
## PATH
############################################################

function prefix_path {
  local dir=$1
  if [ -d "$dir" ] && [[ ":$PATH:" != *":$dir:"* ]]; then
    export PATH="$dir:$PATH"
  fi
}

############################################################
## MANPATH
############################################################

function prefix_manpath {
  local dir=$1
  if [ -d "$dir" ] && [[ ":$MANPATH:" != *":$dir:"* ]]; then
    export MANPATH="$dir:$MANPATH"
  fi
}

############################################################
## Other paths
############################################################

function prefix_cdpath {
  local dir=$1
  if [ -d "$dir" ] && [[ ":$CDPATH:" != *":$dir:"* ]]; then
    export CDPATH="$dir:$CDPATH"
  fi

}

function prefix_ldpath {
  local dir=$1
  if [ -d "$dir" ] && [[ ":$LD_LIBRARY_PATH:" != *":$dir:"* ]]; then
    export LD_LIBRARY_PATH="$dir:$LD_LIBRARY_PATH"
  fi
}

# Convert /home/username or /Users/username to ~/ in PATHs
# Also remove last ':' if any
function homify {
  local path=$1
  # Need to use ',' delimiter instead of '/' in sed since path contains '/'
  echo $path | sed "s,$HOME,~,g" | sed 's/:$//'
}

############################################################
## Optional shell behavior
############################################################

shopt -s cdspell
shopt -s extglob
shopt -s checkwinsize
# shopt -s direxpand # requires special 4.2.24(2) bash direxpand branch
# make bash aliases work in non-interactive bashs
# shopt -s -q expand_aliases # WARNING: breaks Android lunch command

# disable damn flow control (C-s and C-q)
# stty -ixon

export PAGER="less"
# export EDITOR="emacsclient -t"
export EDITOR="vim"

############################################################
## History
############################################################

# When you exit a shell, the history from that session is appended to
# ~/.bash_history.  Without this, you might very well lose the history of entire
# sessions (weird that this is not enabled by default).
shopt -s histappend

# export HISTIGNORE="&:pwd:ls:ll:lal:[bf]g:exit:rm*:sudo rm*"
export HISTIGNORE="pwd:ls:ll:lal:exit"
# remove duplicates from the history (when a new item is added)
export HISTCONTROL=erasedups
# increase the default size from only 1,000 items
export HISTSIZE=1000000

export TERM=xterm-256color
export HOST=$( hostname )

############################################################
## Aliases
############################################################

if [ -e ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

############################################################
## Set PATHs
############################################################
#echo "Old PATH=$PATH"
# Build PATH from scratch
unset PATH
prefix_path /bin
prefix_path /usr/bin
prefix_path /usr/local/bin
prefix_path /sbin
prefix_path /usr/sbin
prefix_path /usr/local/sbin
prefix_path /usr/texbin

prefix_manpath /usr/X11R6/man
prefix_manpath /usr/share/man
prefix_manpath /usr/local/man
prefix_manpath /usr/local/share/man

prefix_cdpath ~/work

unset LD_LIBRARY_PATH
prefix_ldpath /lib
prefix_ldpath /usr/lib
prefix_ldpath /usr/local/X/lib
prefix_ldpath /usr/local/lib

############################################################
## Bash prompt coloring
############################################################

if [ -n "$BASH" ]; then

  # Define a set of colors to help set PS1
  txtblk='\e[0;30m' # Black - Regular
  txtred='\e[0;31m' # Red
  txtgrn='\e[0;32m' # Green
  txtylw='\e[0;33m' # Yellow
  txtblu='\e[0;34m' # Blue
  txtpur='\e[0;35m' # Purple
  txtcyn='\e[0;36m' # Cyan
  txtwht='\e[0;37m' # White
  bldblk='\e[1;30m' # Black - Bold
  bldred='\e[1;31m' # Red
  bldgrn='\e[1;32m' # Green
  bldylw='\e[1;33m' # Yellow
  bldblu='\e[1;34m' # Blue
  bldpur='\e[1;35m' # Purple
  bldcyn='\e[1;36m' # Cyan
  bldwht='\e[1;37m' # White
  unkblk='\e[4;30m' # Black - Underline
  undred='\e[4;31m' # Red
  undgrn='\e[4;32m' # Green
  undylw='\e[4;33m' # Yellow
  undblu='\e[4;34m' # Blue
  undpur='\e[4;35m' # Purple
  undcyn='\e[4;36m' # Cyan
  undwht='\e[4;37m' # White
  bakblk='\e[40m'   # Black - Background
  bakred='\e[41m'   # Red
  badgrn='\e[42m'   # Green
  bakylw='\e[43m'   # Yellow
  bakblu='\e[44m'   # Blue
  bakpur='\e[45m'   # Purple
  bakcyn='\e[46m'   # Cyan
  bakwht='\e[47m'   # White
  txtrst='\e[0m'    # Text Reset

  # export GIT_PS1_SHOWDIRTYSTATE=false
  # export GIT_PS1_SHOWUNTRACKEDFILES=false

  # Disable these for performance (fast cd)
  unset GIT_PS1_SHOWDIRTYSTATE
  unset GIT_PS1_SHOWUNTRACKEDFILES
  export PS1="\[$txtgrn\]\u\[$txtrst\]@\[$bldgrn\]$HOST\[$txtrst\]: \[$txtblu\]\w\[$txtrst\]\[$txtred\]\$(__git_ps1)\n\[$bldred\]\$\[$txtrst\] "
fi

############################################################
## Terminal behavior
############################################################
# Change the window title of X terminals

case $TERM in
  xterm*|rxvt|Eterm|eterm)
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}: ${PWD}\007"'
    ;;
  screen)
    export PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
    ;;
  *)
      # empty
    ;;
esac

# For LS_COLORS template: $ dircolors /etc/DIR_COLORS
#export LS_COLORS="no=00:fi=00:di=01;36:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=01;05;37;41:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.svgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.bz2=01;31:*.tbz2=01;31:*.bz=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.svg=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:"

# Fix interference if we're in Emacs shell
# if [ -n $INSIDE_EMACS ]; then
#   unset PROMPT_COMMAND
# fi

############################################################
## OS independent
############################################################

if [ -e ~/.bashrc_private ]; then
  . ~/.bashrc_private
fi

############################################################
## OS specific
############################################################

case $(uname -s) in
  # Mac OS X
  Darwin)
    if [ -e ~/.bashrc_mac ]; then
      . ~/.bashrc_mac
    fi
    ;;

  Linux)
    if [ -e ~/.bashrc_linux ]; then
      . ~/.bashrc_linux
    fi
    ;;
esac

# Make sure these paths are ahead of custom paths
prefix_path ~/bin
prefix_path ~/bin/private
prefix_path ./bin
# Adding . can be a security issue
# prefix_path .

prefix_manpath ~/man

# Need to investigate this, it broke down the Android build process
# prefix_cdpath .

# Homify paths
export PATH=`homify $PATH`
# Homify MANPATH seems to not load man pages correctly in ~
# export MANPATH=`homify $MANPATH`
# export CDPATH=`homify $CDPATH`
# export LD_LIBRARY_PATH=`homify $LD_LIBRARY_PATH`

############################################################
## Bash Completion, if available
############################################################

# Mac: `brew --prefix` default is /usr/local
if [ -f /usr/local/etc/bash_completion ]; then
 . /usr/local/etc/bash_completion
fi

############################################################
## Other
############################################################

# disable core dumps
ulimit -c 0

# Disable annoying mail notifications
shopt -u mailwarn
unset MAILCHECK

# Default permissions: owner R/W, others no access
umask 077

# Needed to properly display prompt line (user@host ~/folder (git_status)) right after shell starts
# cd .
