my_debclean(){

    OLDCONF=$(dpkg -l|grep "^rc"|awk '{print $2}')
    YELLOW="\033[1;33m"
    RED="\033[0;31m"
    ENDCOLOR="\033[0m"

    if [ $USER != root ]; then
        printf $RED"Error: must be root,exiting...\n$ENDCOLOR"
        exit 1
    fi

    printf "${YELLOW}\nRemoving unused config files...\n$ENDCOLOR"
    apt-get purge $OLDCONF
    printf "${YELLOW}Removing orphan packages\n$ENDCOLOR"
    deborphan | xargs apt-get -y purge

    # in /var/cache/apt/archives
    printf "${YELLOW}remove local packages...\n1) old(default)\n2) all\n3) do nothing\n${RED}Your choice:$ENDCOLOR";read opt;
    case "$opt" in
        "2" ) printf "${YELLOW}remove ${RED}all ${YELLOW}cached packages$ENDCOLOR\n";apt-get clean;;
        "3" ) printf "${YELLOW}no debs would be removed$ENDCOLOR\n";;
        * ) printf "${YELLOW}remove ${RED}old ${YELLOW}cached packages$ENDCOLOR\n";apt-get autoclean;;
    esac
}

my_watchdir () {
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

my_pdf_merge() {
    tomerge="";
    for file in "$@"; do
        tomerge=$tomerge" "$file;
    done
    pdftk $tomerge cat output mergd.pdf;
}
