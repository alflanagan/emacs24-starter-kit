#!/usr/bin/env bash
echo "Removing tangled/downloaded setup files."
cd ~/.emacs.d   # force correct directory
for FILE in *.org
do
    TFILE=${FILE%.org}.el
    if [[ -f ${TFILE} ]]; then
        rm ${TFILE}
    fi
done
rm -rf elpa/*
