#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$PWD/rtags

# if ! [ -d $BUILD ]; then
#     git clone --recursive git@github.com:HongxuChen/rtags.git $SRC
# else
#     cd $SRC
#     git pull --all
#     git submodule update --recursive
# fi

# cd $SRC
# git checkout mine
# cmake $SRC
# make -j 2

ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/g++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/gcc
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang++
ln -sf $SRC ~/.emacs.d/elisp/rtags
