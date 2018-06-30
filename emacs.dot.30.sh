#!/bin/bash

if ! [ -z "$EMACS" ]; then
    echo "Setting up emacs shell environment"
    if [ -d "/server/apps" ]; then
        for directory in /server/apps/*; do
            export PYTHONPATH="$PYTHONPATH:$directory"
        done
    fi
    workon emacs
fi
