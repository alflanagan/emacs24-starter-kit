#!/usr/bin/env bash
#if [[ $# -gt 0 ]]; then  #emacsclient requires file to visit

# detect socket which tells us that an emacs instance is running as a
# server
find_server () {
    local EMACSDIR
    for EMACSDIR in /tmp/emacs*
    do
        local SOCKET=${EMACSDIR}/server
        if [[ -S ${SOCKET} ]]; then
            return ${SOCKET}
        fi
    done
    return ""
}

if [[ ! -z find_server ]]; then
    echo server detected.
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
