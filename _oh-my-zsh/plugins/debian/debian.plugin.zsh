if [[ -e $( which aptitude 2>&1 ) ]]; then
    apt_pref='aptitude'
else
    apt_pref='apt-get'
fi

alias as="aptitude -F \"* %p -> %d \n(%v/%V)\" \
                --no-gui --disable-columns search"      # search package
alias abd='sudo $apt_pref build-dep'
alias ads='sudo apt-get dselect-upgrade'
alias allpkgs='aptitude search -F "%p" --disable-columns ~i'
# Create a basic .deb package
alias mydeb='time dpkg-buildpackage -rfakeroot -us -uc'

apt-copy() {
    print '#!/usr/bin/zsh'"\n" > apt-copy.sh
    cmd='sudo apt-get install -y'
    for p in ${(f)"$(aptitude search -F "%p" --disable-columns \~i)"}; {
        cmd="${cmd} ${p}"
    }
    print $cmd "\n" >> apt-copy.sh
    chmod +x apt-copy.sh
}

apt-history () {
    case "$1" in
    install)
        zgrep --no-filename 'install ' $(ls -rt /var/log/dpkg*)
        ;;
    upgrade|remove)
        zgrep --no-filename $1 $(ls -rt /var/log/dpkg*)
        ;;
    rollback)
        zgrep --no-filename upgrade $(ls -rt /var/log/dpkg*) | \
            grep "$2" -A10000000 | \
            grep "$3" -B10000000 | \
            awk '{print $4"="$5}'
        ;;
    list)
        zcat $(ls -rt /var/log/dpkg*)
        ;;
    *)
        echo "Parameters:"
        echo " install - Lists all packages that have been installed."
        echo " upgrade - Lists all packages that have been upgraded."
        echo " remove - Lists all packages that have been removed."
        echo " rollback - Lists rollback information."
        echo " list - Lists all contains of dpkg logs."
        ;;
    esac
}
