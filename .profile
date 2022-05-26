# loaded by login shells excepts for zsh
if [ -d /usr/local/sbin ]; then
    export PATH=/usr/local/sbin:$PATH
fi

if [ -z "$OSTYPE" ]; then
    OSTYPE="$(uname | tr '[:upper:]' '[:lower:]')"
    export OSTYPE
fi

if [ -n "$BASH_VERSION" ]; then
    case $OSTYPE in
        *darwin*)
            export SHELL=/bin/bash
            export BASH=/bin/bash
            if [ -f /usr/local/etc/bash_completion ]; then
                . /usr/local/etc/bash_completion
            fi ;;
        *) ;;
    esac
    if [ -f "$HOME/.bashrc" ]; then
        # shellcheck source=/dev/null
        . "$HOME/.bashrc"
    fi
fi
