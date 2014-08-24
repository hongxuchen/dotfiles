function zsh_stats() {
    history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

function watchdir () {
    if [[ "$1" != "" ]]; then
        local dir="$1"; shift
        if [[ -x "`which inotifywait`" ]]; then
            ls $dir
            while true; do
                inotifywait -q $@ $dir
            done
        else
            echo "$0: inotifywait not found" > /dev/stderr
        fi
    else
        echo "Usage: $0 <dir> [-e event1 -e event2 ...]"
    fi
}

function mv(){
    FILE="${@: -1}" # bash or ksh,zsh
    if [ -f $FILE ];
    then
        command mv -i $@
    else
        command mv $@
    fi
}

function rc-make(){
    rc -W $1
    make clean
    make -j$(nproc)
}

function dict(){
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

function encode64(){ echo -n $1 | base64 }
function decode64(){ echo -n $1 | base64 -D }

function emacs() {
    ps -C emacs >/dev/null
    if [ $? -eq 0 ];
    then
        if [ $DISPLAY ];
        then emacsclient -c $@ &
        else emacsclient -c $@
        fi
    else
        if [ $DISPLAY ];
        then (nohup emacs -fs $@  >~/.nohup.out) & disown
        else command emacs
        fi
    fi
}

function sudo() {

    if [ "$1" = init ] && [ -n "$SSH_CLIENT" ]; then
        echo >&2 "Never use init when ssh"
        return 1
    else
        command sudo "$@"
    fi
}

function llvmgcc() {
    llvm-gcc -std=c99 -emit-llvm -S $1 -o ${1%.*}.ll ${*:2} -I$HOME/moonbox/klee-install/include
}

function clang-ll() {
    clang -std=c99 -emit-llvm -S $1 -o ${1%.*}.ll ${*:2}
}

# -debug-buffer-size=1024
function opt() {
    command opt -load /home/hongxu/marple-llvm/marple/bin/marple.so $@
}

function clean_llvm(){
    rm -rf klee-* LOG* *.bc *.ll
}

function cmake-ninja(){
    cmake -GNinja $@ && ninja
}

function evince(){
    command evince $@ &>/dev/null &
}

function okular(){
    command okular $@ &>/dev/null &
}

function build_and_run_klee(){
    bc_file=${1%.*}.bc
    llvm-gcc $1 -c -emit-llvm -o ${bc_file}
    klee --max-time=120. -watchdog ${bc_file}
}

# Open the node api for your current version to the optional section.
function node-docs {
    open "http://nodejs.org/docs/$(node --version)/api/all.html#all_$1"
}

# get the name of the ruby version
function rvm_prompt_info() {
  [ -f $HOME/.rvm/bin/rvm-prompt ] || return
  local rvm_prompt
  rvm_prompt=$($HOME/.rvm/bin/rvm-prompt ${ZSH_THEME_RVM_PROMPT_OPTIONS} 2>/dev/null)
  [[ "${rvm_prompt}x" == "x" ]] && return
  echo "${ZSH_THEME_RVM_PROMPT_PREFIX:=(}${rvm_prompt}${ZSH_THEME_RVM_PROMPT_SUFFIX:=)}"
}

pdf-merge() {
  tomerge="";
  for file in "$@"; do
    tomerge=$tomerge" "$file;
  done
  pdftk $tomerge cat output mergd.pdf;
}

function mcd() {
  mkdir -p "$1" && cd "$1";
}

function git-dl(){
    git_url=$1
    folder=${${git_url##*/}%%.*}
    git clone --depth 1 $1 $folder
    rm -rf $folder/.git
}
