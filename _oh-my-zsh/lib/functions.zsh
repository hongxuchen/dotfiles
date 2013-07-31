# -*- mode: sh -*-
function zsh_stats() {
    history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

# snatch stdout of existing process
# see http://subtech.g.hatena.ne.jp/cho45/20091118/1258554176
function snatch() {
    gdb -p $1 -batch -n -x \
        =(echo "p (int)open(\"/proc/$$/fd/1\", 1)
p (int)dup2(\$1, 1)
p (int)dup2(\$1, 2)")
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

function git-set-remote () {
    if [[ "$1" == '-h'  || "$1" == '--help' || "$1" == 'help' ]]; then
        echo "Usage: $0 branch remote"
        return
    fi
    local branch=$1
    local remote=$2
    [[ -z "$branch" ]] &&  branch=master
    [[ -z "$remote" ]] && remote=origin
    git config --add branch.$branch.remote $remote
    git config --add branch.$branch.merge refs/heads/$branch
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

vimhtml() { [[ -f "$1" ]] || return 1; vim +'syn on | run! syntax/2html.vim | wq | q' "$1";}

function emacs() {
    ps -C emacs >/dev/null
    if [ $? -eq 0 ];
    then
        if [ $DISPLAY ];
        then
            emacsclient -c $@ &;
        else
            emacsclient -t $@
        fi
    else
        if [ $DISPLAY ];
        then
            (nohup emacs -fs $@  >~/.nohup.out) & disown
        else
            command emacs -nw
        fi
    fi
}

function sub() {
    local cd="$PWD"
    while [ $1 ]; do
        cd="$(echo $cd | sed "s/$1/$2/")"
        shift; shift
    done
    cd $cd
}

function init() {
    if  [ $SSH_CLIENT ]; then
        echo "don't use init when ssh"
    else
        command init
    fi
}
