#!/bin/bash

# RCol='\e[0m'
# Yel='\e[0;33m'; 

DB="sqlite"
VERSION="2.4.0"
MACHINE="x86_64"

sudo gdebi odb*.deb

for f in *.tar.gz; do
  printf "extracting %s\n" "${f}"
  tar xf "${f}"
done

printf "=%.0s" {1..80}
printf "Installing the Common Runtime Library\n"
cd libodb-${VERSION}
./configure
make
sudo make install
cd ..

printf "=%.0s" {1..80}
printf "Installing the Database Runtime Library(%s)\n" "${DB}"
cd libodb-${DB}-${VERSION}
./configure
make
sudo make install
cd ..

printf "=%.0s" {1..80}
printf "\nInstalling Profile Libraries\n"
cd libodb-boost-${VERSION}
./configure --build=${MACHINE} 
make
sudo make install
cd ..

printf "=%.0s" {1..80}
printf "\nBuilding and Running the Examples\n"
cd odb-examples-${VERSION}
./configure --build=${MACHINE} --with-database ${DB}
make
make check
cd ..

printf "\n=%.0s" {1..80}
printf "\nBuilding and Running the Tests\n"
cd odb-tests-${VERSION}
./configure --build=${MACHINE} --with-database ${DB}
make
make check
cd ..
