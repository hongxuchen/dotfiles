# vim: set ft=zsh ts=2 sw=2 tw=0 et:
# shellcheck disable=SC2296,SC2034,SC2086,SC2016,SC2153,SC2207

ZSH_CACHE_DIR="$ZSH/cache"

autoload -Uz compinit && compinit
compinit -d "${ZSH_CACHE_DIR}/zcomp-$HOST"

## ${ZSH}/completions
ZSH_COMPLETIONS=${ZSH}/completions
fpath=(${fpath} ${ZSH_COMPLETIONS})

unsetopt flowcontrol # no flow control start/end characters
WORDCHARS=''

unsetopt menu_complete   # do not autoselect the first completion entry
setopt auto_menu  # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end
setopt auto_param_slash

zmodload zsh/complist

# :completion:function:completer:command:argument:tag

# zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' completer _extensions _complete
zle -C alias-expension complete-word _generic
bindkey '^Xa' alias-expension
zstyle ':completion:alias-expension:*' completer _expand_alias

# display messages for different matches
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}%d%f'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format '%F{red}No matches for: %d%f'
# zstyle ':completion:*:*:*:*:corrections' format '%F{red}%d%f'

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $(whoami) -o pid,user,comm -w -w"
zstyle ':completion:*:options' list-colors '=^(-- *)=34'

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack
# path-directories: "..", "."
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(|.|..) ]] && reply=(..)'
cdpath=(.)
# // -> /
zstyle ':completion:*' squeeze-slashes true

zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# avoid expanding ~/ --> /home/xxx
zstyle ':completion:*' keep-prefix true

# Use caching so that commands like 'apt' and 'dpkg' complete are useable
zstyle ':completion::complete:*' use-cache 1
# set cache path to store results
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

# zstyle ':completion:*' group-name ''

##############################################################################

_llvm-config() {
    reply=($(llvm-config --help 2>&1 | awk '/  \-/ {print $1}'))
}
compctl -K _llvm-config llvm-config

# _comp_options+=(globdots) # With hidden files
# autoload -Uz compdef

autoload -Uz bashcompinit && bashcompinit
