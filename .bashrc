# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Don't put duplicate lines, and remove all previous lines matching the current
# line from the history list before that line is saved.
# See bash(1) for more options
HISTCONTROL=ignoredups:erasedups

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=50000
HISTFILESIZE=100000

# All commands will have a time stamp
HISTTIMEFORMAT=${HISTTIMEFORMAT:-"%F %H:%M:%S "}

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Use less as terminal pager
export PAGER=less
export LESS='--ignore-case --LONG-PROMPT --RAW-CONTROL-CHARS --window=-4'
export LESS_TERMCAP_so=$'\e[33m\e[7m'
export LESS_TERMCAP_se=$'\e[0m'

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# Git prompt
if [ -f '/usr/local/etc/bash_completion.d/git-prompt.sh' ]; then
    source '/usr/local/etc/bash_completion.d/git-prompt.sh'
elif [ -f '/etc/bash_completion.d/git-prompt' ]; then
    source '/etc/bash_completion.d/git-prompt'
fi

# Prompt with color and Git branch name
if [ -n "$TERM" -a -t ]; then
    _rs=$'\e[0m'
    _bd=$'\e[1m'
    _nr=$'\e[31m'
    _ng=$'\e[32m'
    _nb=$'\e[34m'
    _np=$'\e[35m'

    if [ "$UID" -eq 0 ]; then
        _u="\[$_bd\]\[$_nr\]\u@\h\[$_rs\]"
        _p="\[$_rs\]# "
    else
        _u="\[$_bd\]\[$_ng\]\u@\h\[$_rs\]"
        _p="\[$_rs\]$ "
    fi
    _w="\[$_bd\]\[$_nb\]\w"

    if declare -f __git_ps1 > /dev/null; then
        _g='$(__git_ps1 " '"\[$_rs\]"'('"\[$_ng\]"'%s'"\[$_rs\]"')") '
    else
        _g=' '
    fi
    unset _rs _bd _nr _ng _nb _np
else
    _u="\u@\h"
    _w="\w"
    if [ "$UID" -eq 0 ]; then
        _p='# '
    else
        _p='$ '
    fi

    if declare -f __git_ps1 > /dev/null; then
        _g='$(__git_ps1 " (%s)") '
    else
        _g=' '
    fi
fi

case "$(declare -p PS1 2> /dev/null)" in
    *-x*PS1=*) ;;
    *) PS1="${_u}:${_w}${_g}${_p}" ;;
esac
unset _u _w _g _p

# Set ls aliases
alias l='ls -CF'
alias ll='ls -lahF'
alias lt='ll -rt'

# Set grep with color
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Set interactive rm, cp and mv
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Enable 256color for emacs
if type emacs &> /dev/null; then
    alias emacs='TERM=xterm-256color emacs'
fi

# Enable 256color and specify bash executable for tmux
if type tmux &> /dev/null; then
    alias tmux='tmux -2'
fi

# Force xterm when using ssh to remotely login
alias ssh='TERM=xterm ssh'

# OS-specific aliases / functions
case $OSTYPE in
    *linux*)
        alias ls='ls --color=auto --group-directories-first' ;;
    *darwin*)
        case "$(which ls 2> /dev/null)" in
            *gnubin*)
                alias ls='ls --color=auto --group-directories-first' ;;
            *)
                alias ls='ls -GT'
                export LSCOLORS='ExGxbxdxCxegedabagacad' ;;
        esac
        alias md5sum='md5 -r'
        alias sha1sum='shasum -a 1'
        alias sha256sum='shasum -a 256' ;;
esac
