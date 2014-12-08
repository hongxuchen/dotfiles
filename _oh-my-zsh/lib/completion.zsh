# -*- mode: sh -*-

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu  # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ''

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack
# path-directories: "..", "."
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(|.|..) ]] && reply=(..)'
cdpath=(.)

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH/cache/

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
       adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
       dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
       hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
       mailman mailnull mldonkey mysql nagios \
       named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
       operator pcap postfix postgres privoxy pulse pvm quagga radvd \
       rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs

# ... unless we really want to.
zstyle '*' single-ignored show

##############################################################################

_ninja() {
    reply=(`(ninja -t targets all 2&>/dev/null) | awk -F: '{print $1}'`)
}
compctl -K _ninja ninja

_stp() {
    reply=(`(stp --help) | awk '/-/ {print $1}'`)
}
compctl -K _stp stp

_rc() {
    reply=(`(rc --help) | awk -F'[/|]+' '/-/ {print $1}'`)
}
compctl -K _rc rc

_rdm() {
    reply=(`(rdm --help) | awk -F'[/|]+' '/-/ {print $1}'`)
}
compctl -K _rdm rdm

_llvm-config() {
    reply=(`(llvm-config --help 2>&1) | awk '/  \-/ {print $1}'`)
}
compctl -K _llvm-config llvm-config
