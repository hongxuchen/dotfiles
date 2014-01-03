alias mydeb='time dpkg-buildpackage -rfakeroot -us -uc'
alias belong='apt-file search'

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
