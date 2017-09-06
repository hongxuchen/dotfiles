#!/usr/bin/env bash

function command_exists() {
  command -v "$1" >/dev/null 2>&1
}

if command_exists loc; then
    LOC=loc;
elif command_exists cloc; then
    LOC=cloc;
else
    echo "you need to install 'loc' or 'cloc'"
    exit 1
fi

exclude_lines=$(git submodule | awk '{ print $2 }')
excludes=$(echo -n ${exclude_lines} | tr ' ' '\|')
git ls-files | xargs ${LOC} --exclude "${excludes}"
