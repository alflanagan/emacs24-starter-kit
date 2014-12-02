#!/usr/bin/env bash
ELDIRS="~/.emacs.d ~/.emacs.d/src"

for ELDIR in ${ELDIRS}
do
    for ELFILE in $(eval "ls ${ELDIR}/*.el")
    do
        #TODO: set up array of file names to skip
        if [[ $(basename "${ELFILE}") != ede-projects.el ]]; then
            emacs --batch -f batch-byte-compile "${ELFILE}"
        fi
    done
done
