#!/bin/sh
SRC=$BASEDIR/doxymacs-1.8.0

if ! [ -d $SRC ]; then
    curl http://sourceforge.net/projects/doxymacs/files/latest/download?source=files
    curl http://sourceforge.net/projects/doxymacs/files/doxymacs/1.8.0/doxymacs-1.8.0.tar.gz/download?use_mirror=nchc&r=&amp;ts=1373878446&amp;use_mirror=nchc -o ${SRC}.tar.gz
    tar xvf ${SRC}.tar.gz
fi

BASEDIR="`dirname $0`"
cd $SRC
./configure --prefix=$HOME/.emacs.d/site-lisp/doxymacs --with-default-style=C++
make
make install
