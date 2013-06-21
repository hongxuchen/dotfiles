#!/bin/sh

# issues:
# write permissions
# 

for file in $@;
do
  if [ -f $file ] && ! [ -L $file ]; then
    vim $file -c 'call AppendModeline()' -c 'wq'
  fi
done
