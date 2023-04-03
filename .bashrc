# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# add git branch to promt string
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;95m\] \w\[\033[01;93m\]$(parse_git_branch)\[\033[00m\] $\[\033[00m\] '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h \w $(parse_git_branch) λ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias lla='ls -al'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# general changes
alias apt='sudo apt'
alias g='git '
alias syu='sudo apt update && sudo apt upgrade && sudo apt autoremove'
alias idea='sudo /opt/idea/bin/idea.sh'
alias pkmn='cd ~/quicklisp/local-projects/pkmn'
alias claer='clear'

# work changes
ulimit -n 2048

work=~/code/work
facts=$work/x
pconbox=$work/box

alias x='cd $facts'
alias box='cd $pconbox'

function commits {
    if [ $1 == "by" ]; then
        git log | grep -A5 $2
    fi
}

function update-facts {
    cwd=$(pwd)
    x
    git pull
    git submodule upda9te --init
    cd $cwd
}

function sub {
    if [ $1 == "-s" ]; then
        cd src/submodules/$2
        git status
    else
        cd src/submodules/$1
    fi
}

# note second line might not work
# if so remove the sh -c /dev/null shit
function clean-linux {
    images=$(sudo dpkg --list | grep 'linux-'$1 | awk '{print $2}' | grep -v $(uname -r | awk -F '-amd64' '{print $1}'))
    sudo sh -c 'apt remove -y '$images #> /dev/null 2>&1
}

# requires root
function clean-root {
    # show free sapce
    start=$(sudo df -Th | grep -v fs | awk 'FNR == 2 {print $5}')
    # clean apt
    echo "updating apt repositories..."
    sudo sh -c 'apt update' -y #> /dev/null 2>&1
    echo "upgrading apt repositories..."
    sudo sh -c 'apt upgrade' -y #> #/dev/null 2>&1
    echo "autoremoving orphaned packaged..."
    sudo sh -c 'apt autoremove' -y #> #/dev/null 2>&1
    echo "cleaning apt..."
    sudo sh -c 'apt clean' -y #> /dev/null 2>&1
    echo "cleaning linux images..."
    clean-linux image
    echo "cleaning linux headers..."
    clean-linux headers
    echo "deleting old systemd journals"
    sudo find /var/log -type f -iname *.gz -delete
    sudo journalctl --rotate
    sudo journalctl --vacuum-time=1s

    # show difference
    end=$(sudo df -Th | grep -v fs | awk 'FNR == 2 {print $5}')
    echo ""
    echo "starting space:" $start
    echo "end space:     " $end
}

alias pfacts=update-facts
alias facts='cd ~/code/x ; git status'
alias mycommits='git log | grep -A5 "Robin"'
alias modified='git status | grep modified: | xargs'
setxkbmap -layout gb,de -option 'grp:alt_shift_toggle'

alias ec='emacsclient -c & > /dev/null'
alias et='emacsclient -t'

alias restart-emacs='systemctl restart --user emacs'

alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools

alias emulator='emulator @Pixel_6_API_31'

# mount mac xapp to /mnt/code/xapp
# sshfs app-dev@egrde-mac-rko2.local:/Users/app-dev/_rwh /home/mac/ > /dev/null 2>&1