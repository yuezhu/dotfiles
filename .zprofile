# Do not add duplicates
typeset -U path
typeset -U fpath

# System wide GOROOT
if [[ -d /usr/local/opt/go/libexec ]]; then
    export GOROOT=/usr/local/opt/go/libexec
elif [[ -d /usr/local/go ]]; then
    export GOROOT=/usr/local/go
fi

# Setting $path in the .zshenv does not produce the desired order
# https://stackoverflow.com/questions/15726467/setting-zsh-path-not-producing-desired-order
for p in \
    /usr/local/opt/gawk/libexec/gnubin \
     /usr/local/opt/gnu-sed/libexec/gnubin \
     /usr/local/opt/gnu-indent/libexec/gnubin \
     /usr/local/opt/make/libexec/gnubin \
     /usr/local/opt/findutils/libexec/gnubin \
     /usr/local/bin \
     /usr/local/sbin \
     /usr/local/opt/mysql-client/bin \
     /usr/local/opt/openssl/bin \
     /usr/local/opt/llvm/bin \
     /usr/local/opt/curl/bin \
     /usr/local/opt/terraform@0.12/bin \
     /usr/local/opt/ruby/bin \
     "$GOROOT/bin" \
     "$HOME/bin";
do
    if [[ -d "$p" ]]; then
        path=("$p" $path)
    fi
done

# https://stackoverflow.com/a/52230415/2487942
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
