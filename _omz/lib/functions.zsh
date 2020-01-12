git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

which_exe() {
    exe=$(which $1)    
    rc=$?
    if [[ ${rc} -ne 0 ]]; then
        echo "${exe}"
        return ${rc}
    fi
    exe_path=$(realpath "${exe}")
    ls -lh "${exe_path}"
}

pycdp () {
    cd "$(python3 -c "import os.path as _, ${1}; \
            print(_.dirname(_.realpath(${1}.__file__[:-1])))"
        )"
    if [[ $? != 0 ]]; then
    cd "$(python2 -c "import os.path as _, ${1}; \
            print(_.dirname(_.realpath(${1}.__file__[:-1])))"
        )"
    fi

}

my_cmake_ninja() {
    cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $@ && ninja
}

my_node_docs() {
    open "http://nodejs.org/docs/$(node --version)/api/all.html#all_$1"
}

my_cmp() {
  cmp -l $1 $2 | gawk '{printf "%08X %02X %02X\n", $1, strtonum(0$2), strtonum(0$3)}'
}

## git ignore issues
my_gi() { curl -sL https://www.gitignore.io/api/$@ ;}
_gitignoreio_get_command_list() {
  curl -sL https://www.gitignore.io/api/list | tr "," "\n"
}
_gitignoreio () {
  compset -P '*,'
  compadd -S '' `_gitignoreio_get_command_list`
}
compdef _gitignoreio my_gi

## zsh reload
my_zshreload() {
  local cache=$ZSH_CACHE_DIR
  autoload -U compinit zrecompile
  compinit -d "$cache/zcomp-$HOST"

  for f in ~/.zshrc "$cache/zcomp-$HOST"; do
    zrecompile -p $f && command rm -f $f.zwc.old
  done

  source ~/.zshrc
}

json_pretty() {
  echo "$1" | python -mjson.tool
}

if [[ $OSTYPE == "linux-gnu" ]];then
    source $(dirname "$0")/local_linux
elif [[ $OSTYPE == "darwin"* ]];then
    source $(dirname "$0")/local_darwin
fi
