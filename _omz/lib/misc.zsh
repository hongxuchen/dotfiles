## keybindings
#standard widigets: http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# ^ Ctrl
# \eb Alt
# \e Meta

# "^[[Z" (escape, left bracket, capital z) for shift-tab
bindkey -M emacs '^[[Z' reverse-menu-complete
bindkey -e
################################################################################

## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

## prompty configuration
PS1="%n@%m:%~%# "
PROMPT='%F{green}%2c %F{blue}($(git_prompt_info)%F{blue}) %F{green}%D{%K:%M} %F{yellow}>%f '
ZSH_THEME_GIT_PROMPT_PREFIX="%F{yellow}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%f"
ZSH_THEME_GIT_PROMPT_DIRTY=" %F{red}*%f"
ZSH_THEME_GIT_PROMPT_CLEAN=""

## Command history configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
alias history='fc -l 1 | less'  #fc is a zshbuiltin
setopt long_list_jobs
setopt no_beep
setopt multios
setopt prompt_subst
setopt inc_append_history
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
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

## files and directories
DIRSTACKSIZE=10
setopt auto_cd
# setopt auto_name_dirs
setopt pushd_ignore_dups
setopt pushdminus
setopt auto_pushd

alias ...='cd ../..'
