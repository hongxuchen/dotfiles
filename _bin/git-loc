#!/usr/bin/env bash

function command_exists() {
  command -v "$1" >/dev/null 2>&1
}

if command_exists loc; then
    LOC=loc;
else
    echo "you need to install 'loc'"
    exit 1
fi

exclude_lines=$(git submodule | awk '{ print $2 }')
excludes=""
excludes=$(echo -n ${exclude_lines} | tr ' ' '\|')
if [ -z $excludes ]; then
    git ls-files | xargs loc
else
    git ls-files | xargs loc --exclude "${excludes}"
fi
