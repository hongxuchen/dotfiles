#!/bin/sh
set -ex
BASEDIR="`dirname $0`"
SRC=${BASEDIR}/vim

if ! [ -d $SRC ]; then
    git clone https://github.com/b4winckler/vim $SRC
    cd $SRC
else
    cd $SRC
    git pull --all
fi

git checkout master
./configure  --with-features=huge \
    --enable-rubyinterp \
    --enable-pythoninterp \
    --enable-gui=gtk2 \
    --disable-nls \
    --prefix=/usr \
    # --program-suffix='-chx'
make CC=clang
sudo make install
# sudo checkinstall

sudo update-alternatives --install /usr/bin/editor editor /usr/bin/vim 1
sudo update-alternatives --set editor /usr/bin/vim
sudo update-alternatives --install /usr/bin/vi vi /usr/bin/vim 1
sudo update-alternatives --set vi /usr/bin/vim
