git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

svn_prompt_info() {
    if [ -d .svn ]; then
        rev=$(svn info 2> /dev/null | sed -n 's/Revision:\ //p')
        echo -n "${ZSH_THEME_SVN_PROMPT_PREFIX}${rev}${ZSH_THEME_SVN_PROMPT_SUFFIX}"
    fi
}

em() {
    if [[ $OSTYPE == "linux-gnu" ]] && [ $DISPLAY ]; then
        command emacs -fs $@ &>/dev/null & disown
    else
        command emacs -nw
    fi
}

mkd() {
  mkdir -p "$@" && cd "$1"
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

### TODO change with python
colorless () {
  fname="$1"
  suffix="${fname#*.}"
  pipe_suffix=(a arj tar.bz2 bz bz2 deb udeb ddeb doc gif jpeg jpg pcd png tga tiff tif iso bin raw lha lzh tar.lz tlz lz tar.lzma lzma pdf rar rpm tar.gz tgz tar.z tar.dz tar.xz txz xz gz z dz tar jar war ear xpi zip 7z zoo)
  if (( ${pipe_suffix[(I)${suffix}]} )) || ! [ -e "$fname" ]; then
    command less "$fname"
  else
    pygmentize "$1" | less
  fi
}
###

### git ignore issues
my_gi() { curl -sL https://www.gitignore.io/api/$@ ;}
_gitignoreio_get_command_list() {
  curl -sL https://www.gitignore.io/api/list | tr "," "\n"
}
_gitignoreio () {
  compset -P '*,'
  compadd -S '' `_gitignoreio_get_command_list`
}
compdef _gitignoreio my_gi
###
#
my_docopt_compl(){
  tmpdir="."
  script_name=$1
  compl_name="_${script_name}"
  genfile="${tmpdir}/_${script_name}"
  target_file="${ZSH_COMPLETIONS}/_${script_name}.zsh"
  docopt-completion ${script_name} --manual-zsh
  mv ${genfile} ${target_file}
  source ~/.zshrc
}

### zsh reload
my_zshreload() {
  local cache=$ZSH_CACHE_DIR
  autoload -U compinit zrecompile
  compinit -d "$cache/zcomp-$HOST"

  for f in ~/.zshrc "$cache/zcomp-$HOST"; do
    zrecompile -p $f && command rm -f $f.zwc.old
  done

  source ~/.zshrc
}

pgs() { # [find] [replace] [filename]
    perl -i.orig -pe 's/'"$1"'/'"$2"'/g' "$3"
}

json_pretty() {
  echo "$1" | python -mjson.tool
}

# TODO locale issues

if [[ $OSTYPE == "linux-gnu" ]];then
    source $(dirname "$0")/local_linux
elif [[ $OSTYPE == "darwin"* ]];then
    source $(dirname "$0")/local_darwin
fi
