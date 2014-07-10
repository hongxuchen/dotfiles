#!/bin/sh

# git clone --depth 1 git://git.savannah.gnu.org/emacs.git

build_emacs(){
    set -ex
    BASEDIR="`dirname $0`"
    cd ${BASEDIR}/emacs
    ./configure --disable-largefile  --without-xpm --without-jpeg --without-tiff --without-gif --without-png --without-imagemagick  --without-gconf --without-selinux --without-gsettings --without-gpm --without-makeinfo --with-x-toolkit=yes CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS=-O3 CFLAGS=-O3
# --program-suffix='-chx'
    make -j$(nproc)
    sudo  make install
    cd ${BASEDIR}
}
# --without-toolkit-scroll-bars  --without-xaw3d
build_emacs 2>&1 |tee BUILD_LOG
