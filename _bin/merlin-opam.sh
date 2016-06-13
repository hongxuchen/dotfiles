#!/usr/bin/env bash

# https://gist.github.com/unhammer/c1ac7320141f09ac38e0
# See https://github.com/the-lambda-church/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <opam_package>"
  exit 1
fi

MERLIN='.merlin'
PKG=$1

# printf "\n# opam: PKG=$PKG\n" >> $MERLIN
echo -e "\n" >> $MERLIN
ocamlfind list | awk '{ print "PKG "$1 }'| ag $PKG >> $MERLIN

find ~/.opam -name '*.cmt' -print0 \
| xargs -0 -I{} dirname '{}' \
| sort -u \
| awk '{ print "S "$0"\nB "$0 }' | ag $PKG >> $MERLIN
