#!/bin/sh

git clone git://pwmt.org/zathura-pdf-poppler.git
cd zathura-pdf-poppler
git checkout --track -b develop origin/develop
make
sudo checkinstall

git clone git://pwmt.org/zathura-ps.git
cd zathura-ps
git checkout --track -b develop origin/develop
make
sudo checkinstall

git clone git://pwmt.org/zathura-djvu.git
cd zathura-djvu
git checkout --track -b develop origin/develop
make
sudo checkinstall
