#!/usr/bin/env bash

if [[ "$EUID" -ne 0 ]];
  then echo "Please use sudo or run as root"
  exit
fi

for user in $(cut -f1 -d: /etc/passwd)
do
  echo $user
  crontab -u $user -l
done
