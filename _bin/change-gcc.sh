#!/bin/sh

update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 50
update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 50
update-alternatives --install /usr/bin/cpp cpp-bin /usr/bin/cpp-4.6 50

update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 100
update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 100
update-alternatives --install /usr/bin/cpp cpp-bin /usr/bin/cpp-4.8 100

update-alternatives --set g++ /usr/bin/g++-4.8
update-alternatives --set gcc /usr/bin/gcc-4.8
update-alternatives --set cpp-bin /usr/bin/cpp-4.8
