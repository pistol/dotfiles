# Adds an alias to the current shell and to this file.
# Borrowed from Mislav (http://github.com/mislav/dotfiles/tree/master/bash_aliases)
add-alias ()
{
   local name=$1 value=$2
   echo "alias $name='$value'" >> ~/.bash_aliases
   eval "alias $name='$value'"
   alias $name
}

############################################################
## List
############################################################

alias l="ls"
alias ll="ls -l"
alias la="ls -a"
alias lal="ls -al"

alias lx="ls -lXB"              # sort by extension
alias lk="ls -lSr"              # sort by size
alias lc="ls -lcr"              # sort by change time
alias lu="ls -lur"              # sort by access time
alias lr="ls -laR"              # recursive ls
alias lt="ls -ltr"              # sort by date
alias lm="ls -al |less"         # pipe through 'less'

############################################################
## Emacs
############################################################
# -a '' starts emacs in daemon mode if not already started
alias et="emacsclient -a '' -t"
alias eg="emacsclient -a '' -c -n"
alias ec="emacsclient -n"
alias e="et"
alias ed="emacs --daemon"

############################################################
## Git
############################################################

alias g="git"
alias gb="git branch -a -v"
alias gc="git commit -v"
alias gca="git commit -v -a"
alias gd="git diff"
alias gl="git pull"
alias glr="git pull --rebase"
alias gp="git push"
alias gs="git status -sb"
alias gg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias ggs="gg --stat"
alias gsl="git shortlog -sn"
alias gw="git whatchanged"
alias gsr="git svn rebase"
alias gsp="git svn dcommit"
alias gsu="git submodule update --init --recursive"
alias gi="git config branch.master.remote 'origin'; git config branch.master.merge 'refs/heads/master'"
if [ `which hub 2> /dev/null` ]; then
  alias git="hub"
fi
alias git-churn="git log --pretty="format:" --name-only | grep -vE '^(vendor/|$)' | sort | uniq -c | sort"

# Useful report of what has been committed locally but not yet pushed to another
# branch.  Defaults to the remote origin/master.  The u is supposed to stand for
# undone, unpushed, or something.
function gu {
  local branch=$1
  if [ -z "$1" ]; then
    branch=master
  fi
  if [[ ! "$branch" =~ "/" ]]; then
    branch=origin/$branch
  fi
  local cmd="git cherry -v $branch"
  echo $cmd
  $cmd
}

function gco {
  if [ -z "$1" ]; then
    git checkout master
  else
    git checkout $*
  fi
}

function st {
  if [ -d ".svn" ]; then
    svn status
  else
    git status
  fi
}

############################################################
## Subversion
############################################################

# Remove all .svn folders from directory recursively
alias svn-clean='find . -name .svn -print0 | xargs -0 rm -rf'

############################################################
## Miscellaneous
############################################################

# Display prompt line (user@host ~/folder (git_status)) only when changing dir
# http://superuser.com/questions/33914/why-doesnt-echo-support-e-escape-when-using-the-e-argument-in-macosx
function cd_color {
  cd $*
  echo $'\e[0;32m'$USER$'\e[0m@\e[1;32m'$HOST$': \e[0;34m'$(homify $PWD)$'\e[0;31m'$(__git_ps1)
  # export PS1='\[\e[1;31m\$\e[0m\] '
}
# alias cd='cd_color'

# Display $PATH dirs one per line
function path {
  local path=$1
  : ${path:=$PATH}
  echo $path | tr ':' '\n'
}
alias p='path'
alias type='type -a'
alias t='type'
alias which='type'
alias ..='cd ..'
alias envs='env | sort'

# Fixes color display
alias less='less -R'

alias grep='GREP_COLOR="1;37;41" grep --color=auto'
alias grep='GREP_COLOR="1;37;41" grep --color=auto'
alias wgeto="wget -q -O -"
alias h='history'
alias c="clear"
# Mkdir creates parent dirs in path if missing
alias mkdir='mkdir -p'
# Edit vim settings
alias ve="vim ~/.vimrc"
# Reload bash settings
alias br="source ~/.bashrc"

# # Free space, human readable
alias df='df -h'
alias f='fg'
alias z='suspend'

# List open ports per process
#ports='lsof -i | grep -E "(LISTEN|ESTABLISHED)" | awk '"'"'{print $1, $8, $9}'"'"' | sort -f'
ports='lsof -i | grep -E "(LISTEN|ESTABLISHED)" | sort -fk1'
alias ports="$ports"
alias sports="sudo $ports"

# Test drive speed, argument is # of MB test file
function diskspeed {
  local output=diskspeed.tmp
  local size=$1
  if [ -z $size ] ; then
    size=100
  fi
  dd if=/dev/zero of=diskspeed.tmp bs=1M count=$size
  rm -f $output
}
alias ds='diskspeed'

function speedtest {
  local url='http://speedtest.wdc01.softlayer.com/downloads/test500.zip'
  curl $url -o /dev/null
}

#extract files eg: ex tarball.tar#
function ex {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjfv $1;;
      *.tar.gz)    tar xzfv $1;;
      *.bz2)       bunzip2 $1;;
      *.rar)       rar x $1;;
      *.gz)        gunzip $1;;
      *.tar)       tar xfv $1;;
      *.tbz2)      tar xjfv $1;;
      *.tgz)       tar xzfv $1;;
      *.zip)       unzip $1;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1;;
      *)           echo "'$1' cannot be extracted via extract()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Process listing
#alias ps="ps aux"
function psgrep {
  if [ ! -z $1 ] ; then
    echo "Grepping for processes matching $1..."
    ps aux | grep $1
    #ps aux | grep --color=auto $1 | grep -v grep
  else

    echo "!! Need name to grep for"
  fi
}

# showip - show the current IP address if connected to the internet.
# Usage: showip.
#
function showip {
  lynx -dump -hiddenlinks=ignore -nolist http://checkip.dyndns.org:8245/ | awk '{ print $4 }' | sed '/^$/d; s/^[ ]*//g; s/[ ]*$//g'
}

# Quick webserver for current directory
function serve {
  local port=$1
  : ${port:=3000}
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => $port, :DocumentRoot => Dir.pwd); trap(%q(INT)) { s.shutdown }; s.start"
}

function swap()  # Swap 2 filenames around, if they exist
{                #(from Uzi's bashrc).
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

# Find a file with a pattern in name:
function ff() { find . -type f -iname '*'$*'*' -ls ; }

# Find a file with pattern $1 in name and Execute $2 on it:
function fe()
{ find . -type f -iname '*'${1:-}'*' -exec ${2:-file} {} \;  ; }

# Find a pattern in a set of files and highlight them:
# (needs a recent version of egrep)
function fstr()
{
    OPTIND=1
    local case=""
    local usage="fstr: find string in files.
Usage: fstr [-i] \"pattern\" [\"filename pattern\"] "
    while getopts :it opt
    do
        case "$opt" in
        i) case="-i " ;;
        *) echo "$usage"; return;;
        esac
    done
    shift $(( $OPTIND - 1 ))
    if [ "$#" -lt 1 ]; then
        echo "$usage"
        return;
    fi
    find . -type f -name "${2:-*}" -print0 | \
    xargs -0 egrep --color=always -sn ${case} "$1" 2>&- | more

}

############################################################
# Private aliases (OS independent)
############################################################

if [ -e ~/.bash_aliases_private ]; then
  . ~/.bash_aliases_private
fi

############################################################
# OS specific aliases (Mac or Linux)
############################################################

case $(uname -s) in
  # Mac OS X
  Darwin)
    if [ -e ~/.bash_aliases_mac ]; then
      . ~/.bash_aliases_mac
    fi
    ;;

  Linux)
    if [ -e ~/.bash_aliases_linux ]; then
      . ~/.bash_aliases_linux
    fi
    ;;
esac

############################################################
