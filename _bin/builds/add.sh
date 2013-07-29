#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$PWD/cc-lookup

if ! [ -d $SRC ]; then
    git clone git@github.com:HongxuChen/cc-lookup.git
    cd $SRC
else
    cd $SRC
    git pull --all
fi

git checkout develop
make LLVM_CONFIG=llvm-config-3.4

ln -sf $SRC ~/.emacs.d/site-lisp/cc-lookup
