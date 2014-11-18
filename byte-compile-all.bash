#!/usr/bin/env bash
ELDIRS="~/.emacs.d ~/.emacs.d/src"

for ELDIR in ${ELDIRS}
do
    for ELFILE in $(eval "ls ${ELDIR}/*.el")
    do
        emacs --batch --eval "(byte-compile-file \"${ELFILE}\")"
    done
done
