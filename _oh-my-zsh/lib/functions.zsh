git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

dict(){
    word=$1
    TEMP="/tmp/dict.${word}"
    MEMO=~/.dict/`date +%G%m`
    command dict $word 1>$TEMP
    if [ -s $TEMP ]
    then
        less $TEMP
        echo $word >> $MEMO
        sort -u $MEMO -o $MEMO
    else
        echo 'spell error?'
    fi
}

emacs() {
    if [[ $OSTYPE == "linux-gnu" ]] && [ $DISPLAY ]; then
        command emacs -fs $@ &>/dev/null & disown
    else
        command emacs -nw
    fi
}

cmake_ninja(){
    cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $@ && ninja
}

# Open the node api for your current version to the optional section.
my_node_docs() {
    open "http://nodejs.org/docs/$(node --version)/api/all.html#all_$1"
}

my_git_dl(){
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

my_zeal(){
    if pgrep -x zeal &>/dev/null; then
        printf "already on\n"
    else
        ~/tools/zeal/zeal/zeal &>/dev/null &
    fi
}

my_locals(){
    locale -a |
    grep _ | #don't show nationalities
    uniq -w5 | #merge available charmaps
    while read lang; do
        echo -ne "$lang\t";
        locale_info=`LANG=$lang locale territory language 2>/dev/null`
        echo $locale_info | sed 's/\(.*\) \(.*\)/\1 (\2)/'
    done |
    sort -k2
}

if [[ $OSTYPE == "linux-gnu" ]];then
    source $(dirname $0)/local_linux.zsh
elif [[ $OSTYPE == "darwin"* ]];then
    source $(dirname $0)/local_darwin.zsh
fi
