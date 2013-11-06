#!/bin/bash

brew update

brew install \
    coreutils \
    git \
    python \
    python3 \
    tmux \
    tree \
    irssi \
    ack \
    wget \
    bash-completion \
    macvim \
    emacs \
    postgresql \
    z

if [ ! -d ~/sh ]; then
    echo "~/sh not found, creating..."
    mkdir -v ~/sh
fi

# rupa/z
if [ ! -f  ~/sh/z.sh ]; then
    echo "~/sh/z.sh not found, creating..."
    cp -v $(brew --prefix)/etc/profile.d/z.sh ~/sh/z.sh
fi
