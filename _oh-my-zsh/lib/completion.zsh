# -*- mode: sh -*-

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu  # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end
set auto_param_slash

WORDCHARS=''

zmodload zsh/complist

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ''

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $(whoami) -o pid,user,comm -w -w"

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

_dropbox() {
  reply=(`python <<END
import subprocess
help_msg = subprocess.check_output(['dropbox', 'help'])
i1 = help_msg.find('Note:')
i2= help_msg[i1:].find('.')
cli_msg = help_msg[i1+i2+1:]
cmd = []
for line in cli_msg.splitlines():
  line = line.strip()
  if line:
    cmd.append(line.split(None, 1)[0])
res = '\n'.join(cmd)
print res
END`)
}
compctl -K _dropbox dropbox


### TODO should use zsh features
### for argcomplete: https://github.com/kislyuk/argcomplete
# activate-global-python-argcomplete is not needed for zsh
autoload -Uz bashcompinit
bashcompinit
eval "$(register-python-argcomplete ~/.bin/shebang.py)"
###
