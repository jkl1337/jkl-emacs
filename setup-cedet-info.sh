#!/bin/sh

INFODIR=/home/jluebs/emacs/info/dir
CEDET=cedet
cd "$CEDET"

find . -name '*.info' | while read infofile; do
    install-info "$infofile" "$INFODIR"
done
