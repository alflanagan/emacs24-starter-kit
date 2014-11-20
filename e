#!/usr/bin/env bash

# detect socket which tells us that an emacs instance is running as a
# server
find_server () {
    for EMACSDIR in /tmp/emacs*
    do
        SOCKET=${EMACSDIR}/server
        if [[ -S ${SOCKET} ]]; then
            return 1
        fi
    done
    return 0
}

find_server

if [[ $? -eq 1 ]]; then
    if [[ $# -lt 1 ]]; then
        echo emacs is already running.
        exit 1
    else
        echo calling emacsclient.
        emacsclient -n "$@" > ~/log/emacsclient.log 2>&1 &
    fi
else
    echo server not found.
    emacs "$@" > ~/log/emacs.log 2>&1 &
fi
