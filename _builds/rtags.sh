#!/bin/bash

BASEDIR=$(dirname "$0")


cd "$BASEDIR"
SRC=$(pwd)/rtags
BUILD=$SRC/../build
ELISP=$HOME/.emacs.d/config/rtags

if ! [ -d "$SRC" ]; then
    git clone --recursive git@github.com:Andersbakken/rtags.git  "$SRC"
else
    cd "$SRC"
    git pull --all
    git submodule update --recursive
fi

rm "$ELISP"
git checkout master

rm -rf "$BUILD" && mkdir -p "$BUILD" && cd "$BUILD"
export C_INCLUDE_PATH=
export CPLUS_INCLUDE_PATH=
export LD_LIBRARY_PATH=
export PATH=/usr/bin:$PATH
export CC=gcc
export CXX=g++
# cmake -GNinja "$SRC"
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Release "$SRC"
make -j
sudo make install

ln -sf "$SRC" "$ELISP"
