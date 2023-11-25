# vim: set ft=zsh ts=2 sw=0 tw=0 et:
#shellcheck disable=SC2034

## keybindings
#standard widigets: http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# ^ Ctrl
# \eb Alt
# \e Meta

# "^[[Z" (escape, left bracket, capital z) for shift-tab
bindkey -M emacs '^[[Z' reverse-menu-complete
# force emacs-like key bindings, otherwise it will guess from $EDITOR/$VISUAL
bindkey -e
################################################################################

## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

bindkey '^Q' push-line-or-edit

## prompty configuration
_git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$MY_ZSH_GIT_PROMPT_PREFIX${ref#refs/heads/}$MY_ZSH_GIT_PROMPT_SUFFIX"
}
PS1="%n@%m:%~%# "
#shellcheck disable=SC2016
PROMPT='%F{blue}%D{%K:%M} $(_git_prompt_info)%F{green}%2c%F{yellow}$%f '
MY_ZSH_GIT_PROMPT_PREFIX="%F{blue}(%F{yellow}"
MY_ZSH_GIT_PROMPT_SUFFIX="%F{blue})%f "

## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
alias history='fc -l 1 | pager'  #fc is a zshbuiltin
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

unsetopt extended_glob
unsetopt glob_dots
setopt csh_null_glob # do not report errors at least there is one match

[[ -e /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

## files and directories
DIRSTACKSIZE=10
setopt cd_silent
setopt auto_cd # '$DIR' -> 'cd $DIR'
setopt auto_pushd # make 'cd' push to dir stack
setopt pushd_ignore_dups # don't push duplicated dirs
setopt pushd_minus # exchange meaning of +/-
unsetopt cdablevarS
unsetopt auto_name_dirs # always named with regular dir names
alias ...='cd ../..'
alias ....='cd ../../..'

alias mmv='noglob zmv -W'
