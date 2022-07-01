## Ensure uniqueness for path and fpath arrays
typeset -U path
typeset -U fpath

## PATH
# On macOS, setting $path in the .zshenv does not produce the desired order
# https://stackoverflow.com/a/63344431
for d in \
  /usr/local/sbin /usr/local/bin;
do
  if [[ -d "${d}" ]]; then
    path=("${d}" $path)
  fi
done

for d in \
  /usr/local/opt/go/libexec /usr/local/go;
do
  if [[ -d "${d}" ]]; then
    export GOROOT="${d}"
    path=("${GOROOT}/bin" $path)
    break
  fi
done

for d in \
  /usr/local/opt/gawk/libexec/gnubin \
    /usr/local/opt/gnu-sed/libexec/gnubin \
    /usr/local/opt/gnu-indent/libexec/gnubin \
    /usr/local/opt/make/libexec/gnubin \
    /usr/local/opt/findutils/libexec/gnubin \
    /usr/local/opt/mysql-client/bin \
    /usr/local/opt/openssl/bin \
    /usr/local/opt/llvm/bin \
    /usr/local/opt/curl/bin \
    /usr/local/opt/ruby/bin;
do
  if [[ -d "${d}" ]]; then
    path=("${d}" $path)
  fi
done

for d in \
  "${HOME}/bin" "${HOME}/Library/Mobile Documents/com~apple~CloudDocs/bin";
do
  if [[ -d "${d}" ]]; then
    path=("${d}" $path)
  fi
done

## zsh-autosuggestions
for d in \
  "${HOME}/.nix-profile/share/zsh-autosuggestions" \
    /usr/local/share/zsh-autosuggestions \
    /usr/share/zsh-autosuggestions;
do
  if [[ -d "${d}" ]]; then
    source "${d}/zsh-autosuggestions.zsh"
    break
  fi
done

## zsh-syntax-highlighting
for d in \
  "${HOME}/.nix-profile/share/zsh-syntax-highlighting" \
    /usr/local/share/zsh-syntax-highlighting \
    /usr/share/zsh-syntax-highlighting;
do
  if [[ -d "${d}" ]]; then
    export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR="${d}/highlighters"
    source "${d}/zsh-syntax-highlighting.zsh"
    break
  fi
done

## Additional completions
for d in \
  "${HOME}/.nix-profile/share/zsh/site-functions" \
    /usr/local/share/zsh-completions;
do
  if [[ -d "${d}" ]]; then
    fpath=("${d}" $fpath)
  fi
done

# Enable colored menu and scrolling completion in completion listings
# This needs to be loaded before calling compinit.
# http://zsh.sourceforge.net/Doc/Release/Zsh-Modules.html#The-zsh_002fcomplist-Module
zmodload -i zsh/complist

## Completion
autoload -Uz compinit

# Speed up zsh compinit by only checking cache once a day
#
# The option -u makes compinit skip security check and all files found be
# used.
#
# The option -C skips the check performed to see if there are new functions
# can be omitted. In this case the dump file will only be created if there
# isn't one already.
#
# Use an anonymous function that takes a file name as an argument using glob
# qualifiers (N.mh+24) when EXTENDED_GLOB is not enabled. In contrast, if
# EXTENDED_GLOB is enabled, globbing will be performed for [[ -n
# ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]:
#
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's
#   [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match
#   (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+24' matches files (or directories or whatever) that are older than 24
#   hours.
# https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-3109177
ZSH_COMPDUMP=${ZDOTDIR:-$HOME}/.zcompdump
() {
  if [[ $# -gt 0 ]]; then
    compinit -u
  else
    compinit -C
  fi
} ${ZSH_COMPDUMP}(N.mh+24)

# http://zsh.sourceforge.net/Doc/Release/Options.html
# 16.2.2 Completion
# If a completion is performed with the cursor within a word, and a full
# completion is inserted, the cursor is moved to the end of the word. That is,
# the cursor is moved to the end of the word if either a single match is
# inserted or menu completion is performed.
setopt ALWAYS_TO_END

# If unset, the cursor is set to the end of the word if completion is
# started. Otherwise it stays there and completion is done from both ends.
setopt COMPLETE_IN_WORD

# When listing files that are possible completions, show the type of each file
# with a trailing identifying mark, like the -F option to ls.
setopt LIST_TYPES

# Lay out the matches in completion lists sorted horizontally, that is, the
# second match is to the right of the first one, not under it as usual.
setopt LIST_ROWS_FIRST

# On an ambiguous completion, instead of listing possibilities or beeping,
# insert the first match immediately.
# This causes the current candidate to be selected and inserted immediately
# without having to press TAB.
# setopt MENU_COMPLETE

# Try to make the completion list smaller (occupying less lines) by printing
# the matches in columns with different widths.
setopt LIST_PACKED

# Export LS_COLORS
for f in \
  /usr/local/bin/gdircolors /usr/bin/dircolors;
do
  if [[ -x "${f}" ]]; then
    eval "$("${f}" -b)"
    break
  fi
done

# Display lists of matches for files in different colours depending on the file
# type
# 22.7.1 Colored completion listings
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Enable scrolling through a completion list
# 22.7.2 Scrolling in completion listings
zstyle ':completion:*:default' list-prompt ''

# Enable menu selection
# Display a list of candidates for an ambiguous completion when hitting TAB,
# and start menu selection when hitting TAB again.
zstyle ':completion:*' menu select

# Matches in the same group are shown together
zstyle ':completion:*' group-name ''

# Enable completion for `.' and `..' special directories
zstyle ':completion:*' special-dirs true

# Directories to be completed are listed separately from and before completion
# for other files, regardless of tag ordering.
zstyle ':completion:*' list-dirs-first true

# Describe completion
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*:corrections' format '%F{yellow}——————— %d (errors: %e) ———————%f'
zstyle ':completion:*:descriptions' format '%F{green}——————— %d ———————%f'
zstyle ':completion:*:messages' format '%F{white}——————— %d ———————%f'
zstyle ':completion:*:warnings' format '%F{red}——————— no completions ———————%f'

# Allow fuzzy and `*' matching completion
zstyle ':completion:*' completer _complete _match _approximate

# Remove duplicate matches for _history completer
zstyle ':completion:*' remove-all-dups true

# Highlight the first ambiguous character in completion lists
zstyle ':completion:*' show-ambiguity true

# When `*' is present, only matching `*' and no other completion is made.
zstyle ':completion:*:match:*' original only

# Increase the number of errors based on the length of the typed word
# zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*' max-errors 1

# Ignore shell functions that should not be used individually
zstyle ':completion:*:functions' ignored-patterns '_*'

# Ignore completion for uninterested users
zstyle ':completion:*:*:*:users' ignored-patterns '_*'

# Ignore backup files when completing commands
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Reorder completion for tilde
zstyle ':completion::*:-tilde-:*:*' group-order named-directories users directory-stack

# Kill command completion
zstyle ':completion:*:*:kill:*:processes' command 'ps -U ${USERNAME} -o pid,user,command | sed "/ps -U '${USERNAME}'/d"'

# Colorize kill completion menu
zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]##) ([0-9a-z_-]##) *=0=31=36'

bindkey -M menuselect '^s' history-incremental-search-forward

## History
HISTSIZE=1000000
SAVEHIST="$HISTSIZE"
HISTFILE="${HOME}/.zsh_history"

# This option both imports new commands from the history file, and also causes
# your typed commands to be appended to the history file (the latter is like
# specifying INC_APPEND_HISTORY, which should be turned off if this option is in
# effect). The history lines are also output with timestamps ala
# EXTENDED_HISTORY (which makes it easier to find the spot where we left off
# reading the file after it gets re-written).
setopt SHARE_HISTORY

# If a new command line being added to the history list duplicates an older one,
# the older command is removed from the list (even if it is not the previous
# event).
setopt HIST_IGNORE_ALL_DUPS

# Remove superfluous blanks from each command line being added to the history
# list.
setopt HIST_REDUCE_BLANKS

# Save each command's beginning timestamp (in seconds since the epoch) and the
# duration (in seconds) to the history file.
setopt EXTENDED_HISTORY

## Prompt
autoload -Uz colors
colors

# Allow dynamic command prompt
# Substitutions within prompts do not affect the command status.
setopt PROMPT_SUBST

autoload -Uz add-zsh-hook

# Enable VCS info
autoload -Uz vcs_info

# Only enable for a few frequently used VCS tools
zstyle ':vcs_info:*' enable git

# Update each time new prompt is rendered
add-zsh-hook precmd vcs_info

# Minimal VCS information in prompt
zstyle ':vcs_info:git:*' formats ' %B%F{cyan}%s:%b%%b%f'
zstyle ':vcs_info:git:*' actionformats ' %B%F{cyan}%s:%b|%a%%b%f'

# https://github.com/ohmyzsh/ohmyzsh/blob/master/themes/bira.zsh-theme
PROMPT='%B%F{green}%m%f %F{blue}%~%f%F{cyan}${vcs_info_msg_0_}%f %(!.#.$)%b '
#RPROMPT=' %B%D{%H:%M:%S.%.}%b'

## MISC
# Report command running time if its user/system takes longer than 3 seconds
REPORTTIME=3

# 16.2.6 Input/Output
# Print the exit value of programs with non-zero exit status.
# 09/01/21 disabled becuase this's misleading for some cases.
# setopt PRINT_EXIT_VALUE

# Try to correct the spelling of all arguments in a line.
# The shell variable CORRECT_IGNORE_FILE may be set to a pattern to match file
# names that will never be offered as corrections.
# setopt CORRECT_ALL
setopt NO_CORRECT_ALL

# Disable flow control so that the keybindings ^S/^Q can be assigned.
setopt NO_FLOW_CONTROL

# Allow comments even in interactive shells
setopt INTERACTIVE_COMMENTS

## Terminal titles
# https://wiki.archlinux.org/title/zsh#xterm_title
# For expansion definition:
# https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html#Prompt-Expansion
function xterm_title_precmd () {
  print -Pn -- '\e]2;%m: %~\a'
}

add-zsh-hook -Uz precmd xterm_title_precmd

## Use less as terminal pager
export PAGER=less
export LESS='--ignore-case --LONG-PROMPT --RAW-CONTROL-CHARS --window=-4'
export LESS_TERMCAP_so=$'\e[33m\e[7m'
export LESS_TERMCAP_se=$'\e[0m'

# Configure pinentry to use the correct TTY
export GPG_TTY=$(tty)

## Aliases
case $OSTYPE in
  *linux*)
    alias ls='ls --color=auto --group-directories-first' ;;
  *darwin*)
    case $(which ls 2> /dev/null) in
      *gnubin*)
        alias ls='ls --color=auto --group-directories-first' ;;
      *)
        alias ls='ls -GT'
        export CLICOLOR=1
        export LSCOLORS='ExGxbxdxCxegedabagacad' ;;
    esac
    alias htop='sudo /usr/local/bin/htop' ;;
esac

alias l='ls -CF'
alias ll='ls -lahF'
alias lt='ll -rt'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias history='history -i'

## Additional customization
if [[ -f ~/.zsh_custom ]]; then
  . ~/.zsh_custom
fi
