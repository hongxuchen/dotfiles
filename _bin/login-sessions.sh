#!/usr/bin/env sh

mysessions=$(loginctl list-sessions | grep -w "$(whoami)" | awk '{print $1}')

for session in $mysessions; do
    echo "===> Session $session"
    loginctl show-session $session
done
