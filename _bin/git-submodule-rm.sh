#!/bin/bash

#------------------------------------------------
# taken from https://gist.github.com/2491147
#------------------------------------------------

function actual_path() {
  if [ [ -z "$1" ] -a [ -d $1 ] ]; then
# `pwd -P` gets the physical location
    echo $(cd $1 && test `pwd` = `pwd -P`)
    return 0
  else
    return 1
  fi
}

function is_submodule() {
    # local variables
    local top_level parent_git module_name

    if [ -d "$1" ]; then
      cd $1
    else
      return 1
    fi

    # Find the root of this git repo, then check if its parent dir is also a repo
    # here parent should not be a git repo!
    top_level="$(git rev-parse --show-toplevel)"
    if [ ! actual_path $toplevel ]; then
      top_level="$(cd $top_level && pwd -P)"
    fi
    # not used, only to show its the top_level repo name
    module_name="$(basename "$top_level")"
    # this top_level repo's parent is not a git repo,it is expected to report an error to stderr
    parent_git="$(cd "$top_level/.." && git rev-parse --show-toplevel 2> /dev/null)"

    if [[ -n $parent_git ]]; then
      return 0
    else
      return 1
    fi
}

function is_gitroot() {
  if [ "$(pwd -P)" = "$(git rev-parse --show-toplevel)" ]; then
    return 0
  else
    return 1
  fi
}

# first check if it's a valid path(exists and is directory)
if [ ! -d "$1" ]; then
  echo "Usage: git submodule rm <path>"
  exit
fi

# then check whether we're at git root
if is_gitroot; then
  # finally check whether the given path is a submodule
  # note that normal return status should be 0
  if $(is_submodule "${1}"); then
    echo "let's remove those submodules"
    # using ${1%/} to remove trailing slashes
    git config -f .gitmodules --remove-section submodule.${1%/}
    git config -f .git/config --remove-section submodule.${1%/}
    git rm --cached ${1%/}
  else
    echo "git submodule rm is not recursive yet, aborting."
  fi
else
  echo "You need to run this command from the toplevel of the working tree."
fi
