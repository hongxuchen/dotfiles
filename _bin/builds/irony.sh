#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$PWD/irony

if ! [ -d $SRC/build ]; then
    mkdir -p $SRC/build
    cd $SRC
    git clone --recursive git@github.com:HongxuChen/irony-mode.git irony-mode
else
    cd $SRC
    git pull --all
    git submodule update --recursive
fi

git checkout mine
cd $SRC/build
cmake $SRC/irony-mode
make -j 2
make install

ln -sf $SRC ~/.emacs.d/elisp/irony
