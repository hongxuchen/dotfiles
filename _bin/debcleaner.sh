#################################################################
#debian based cleaner
#1. purge unused packages
#2. clean trash files
#requirements
#deborphan
#aptitude
#trash-cli(optional)
#################################################################


#!/bin/sh

PROG=$0
OPTIONS=`getopt -o pth -l package,trash,help -- $*`
OLDCONF=$(dpkg -l|grep "^rc"|awk '{print $2}')
CURKERNEL=$(uname -r|sed 's/-*[a-z]//g'|sed 's/-386//g')
LINUXPKG="linux-(image|headers|ubuntu-modules|restricted-modules)"
METALINUXPKG="linux-(image|headers|restricted-modules)-(generic|i386|server|common|rt|xen)"
OLDKERNELS=$(dpkg -l|awk '{print $2}'|grep -E $LINUXPKG |grep -vE $METALINUXPKG|grep -v $CURKERNEL)
YELLOW="\033[1;33m"
RED="\033[0;31m"
ENDCOLOR="\033[0m"

clean_packages(){
printf "${YELLOW}\nRemoving old kernels...$ENDCOLOR"
aptitude purge $OLDKERNELS
printf "${YELLOW}\nRemoving unused config files...\n$ENDCOLOR"
aptitude purge $OLDCONF
printf "${YELLOW}Removing orphan packages\n$ENDCOLOR"
deborphan | xargs aptitude -y purge
}

remove_files(){
#cached debs in /var/cache/apt/archives
printf "${YELLOW}remove local packages...\n1) old(default)\n2) all\n3) do nothing\n${RED}Your choice:$ENDCOLOR";read opt;
case "$opt" in
    "2" ) printf "${YELLOW}remove ${RED}all ${YELLOW}cached packages$ENDCOLOR\n";aptitude clean;;
"3" ) printf "${YELLOW}no debs would be removed$ENDCOLOR\n";;
* ) printf "${YELLOW}remove ${RED}old ${YELLOW}cached packages$ENDCOLOR\n";aptitude autoclean;;
esac

#trash in GTK based environment
printf "${YELLOW}\nEmptying trashes...\n$ENDCOLOR"
TRASH=`dpkg-query -W -f='${Package}\n' "trash-cli" 2>&1 |awk '{print $1,$2,$3,$4}'`
if [ $TRASH = "No packages found matching" ];
then
    rm -rf ~/.local/share/Trash/*/** &> /dev/null
    rm -rf /root/.local/share/Trash/*/** &> /dev/null
else
    trash-list; trash-empty
fi
}

print_help(){
    printf "usage: $0 [option]\n"
    printf "option:\n\t-h,--help\t\t\tprint this help\n\t-t,--trash\t\t\tempty trash\n\t-p,--package\t\t\tclean packages\n"
}

deb_clean(){
if [ $USER != root ]; then
    printf $RED"Error: must be root,exiting...\n$ENDCOLOR"
    exit 1
fi

if [ $# -eq 0 ];
then
    clean_packages
    remove_files
    return
fi

if [ $# != 0 ]; then printf "getopt error"; fi
eval set -- "$OPTIONS"
printf "${RED}--------------------------------------------------\n$ENDCOLOR"
while true
do
    case $1 in
        -h|--help)
            print_help
            break
            ;;
        -p|--package)
            clean_packages
            ;;
        -t|--trash)
            remove_files
            ;;
        *)
            break;
            ;;
    esac
    shift
done

printf "${RED}--------------------------------------------------\n$ENDCOLOR"
}

deb_clean
