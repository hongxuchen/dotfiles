## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

## Command history configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
alias history='fc -l 1 | less'  #fc is a zshbuiltin

# ls colors
# autoload colors; colors;
# export LSCOLORS="Gxfxcxdxbxegedabagacad"
export LSCOLORS=${LS_COLORS}

PS1="%n@%m:%~%# "

# git theming default: Variables for theming the git info prompt
# ZSH_THEME_GIT_PROMPT_PREFIX="git:("         # Prefix at the very beginning of the prompt, before the branch name
# ZSH_THEME_GIT_PROMPT_SUFFIX=")"             # At the very end of the prompt
# ZSH_THEME_GIT_PROMPT_DIRTY="*"              # Text to display if the branch is dirty
# ZSH_THEME_GIT_PROMPT_CLEAN=""               # Text to display if the branch is clean

PROMPT='%F{green}%2c%F{blue} >%f '
RPROMPT='$(git_prompt_info) %F{blue}< %F{green}%D{%L:%M} %F{yellow}%D{%p}%f'
# RPROMPT='$(git_prompt_info)$(svn_prompt_info) %F{blue}< %F{green}%D{%L:%M} %F{yellow}%D{%p}%f'

ZSH_THEME_GIT_PROMPT_PREFIX="%F{yellow}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%f"
ZSH_THEME_GIT_PROMPT_DIRTY=" %F{red}*%f"
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_SVN_PROMPT_PREFIX="[%{$reset_color%}%{$fg[white]%}svn:%{$fg_bold[white]%}/"
ZSH_THEME_SVN_PROMPT_SUFFIX="%{$fg_bold[green]%}]-"

setopt long_list_jobs
setopt no_beep
setopt multios
setopt prompt_subst
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
setopt hist_ignore_all_dups
setopt hist_no_store
setopt hist_reduce_blanks
setopt hist_verify
setopt hist_beep
# setopt extended_glob
# setopt cdablevarS
# setopt nohashdirs #immediately $PATH executables

[[ -e /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found

alias -g L=' | less'
alias -g LL='2>&1 | v -'
alias -g HH='| head -n 20'
alias -g TT='| tail -20'
TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

autoload -U age

##### files and directories
DIRSTACKSIZE=10
setopt auto_cd
# setopt auto_name_dirs
setopt pushd_ignore_dups
setopt pushdminus
setopt auto_pushd
alias ...='cd ../..'

alias -s deb="sudo gdebi"
alias -s jar="java -jar"
alias -s plist="plutil"
