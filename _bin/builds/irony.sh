#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR

SRC=$(pwd)/irony-mode
BUILD=$SRC/build

if ! [ -d $BUILD ]; then
    git clone --recursive git@github.com:HongxuChen/irony-mode.git $SRC
    mkdir -p $BUILD
else
    cd $SRC
    git pull --all
    git submodule update --recursive
fi

git checkout mine
cd $BUILD
cmake -GNinja $SRC
ninja

ln -sfT $SRC ~/.emacs.d/elisp/irony-mode
