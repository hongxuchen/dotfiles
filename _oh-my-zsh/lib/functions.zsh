git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# handy operations

my_emacs() {
    if [[ $OSTYPE == "linux-gnu" ]] && [ $DISPLAY ]; then
        command emacs -fs $@ &>/dev/null & disown
    else
        command emacs -nw
    fi
}

my_cmake_ninja() {
    cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $@ && ninja
}

my_node_docs() {
    open "http://nodejs.org/docs/$(node --version)/api/all.html#all_$1"
}

my_git_dl() {
    git_url=$1
    folder=${${git_url##*/}%%.*}
    git clone --depth 1 $1 $folder
    rm -rf $folder/.git
}

my_brew_backup () {
    echo '#!/bin/bash'
    echo ''
    echo 'failed_items=""'
    echo 'install_package() {'
    echo 'echo EXECUTING: brew install $1 $2'
    echo 'brew install $1 $2'
    echo '[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.'
    echo '}'

    brew tap | while read tap; do echo "brew tap $tap"; done
    brew list | while read item;
                do
                    echo "install_package $item '$(brew info $item | grep 'Built from source with:' | sed 's/^[ \t]*Built from source with:/ /g; s/\,/ /g')'"
                done
    echo '[ ! -z $failed_items ] && echo The following items were failed to install: && echo $failed_items'

}

if [[ $OSTYPE == "linux-gnu" ]];then
    source $(dirname $0)/local_linux
elif [[ $OSTYPE == "darwin"* ]];then
    source $(dirname $0)/local_darwin
fi
