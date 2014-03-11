#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$PWD/clang-faces
BUILD=$SRC/build

if ! [ -d $BUILD ]; then
    git clone git@bitbucket.org:HongxuChen/clang-faces.git
    mkdir -p $BUILD
else
    cd $SRC
    git pull --all
fi

cd $BUILD
cmake $SRC
make -j 2

ln -sf $SRC ~/.emacs.d/site-lisp/clang-faces
