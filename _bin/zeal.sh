#!/bin/bash

if pgrep -x zeal &>/dev/null; then
    printf "already on\n"
else
    ~/tools/zeal/zeal/zeal &>/dev/null &
fi
