## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## file rename magick
bindkey "^[m" copy-prev-shell-word

## jobs
setopt long_list_jobs

## directories
# setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups

## edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

alias history='fc -l 1'  #fc is a zshbuiltin

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

[[ -e /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found

if [[ $OSTYPE == "linux-gnu" ]]; then
	for s in mp3 wav aac \
		ogg avi mp4 m4v mov qt mpg mpeg \
		jpg jpeg png psd bmp gif tif tiff \
		eps ps pdf html dmg; do
		alias -s $s=xdg-open
	done
fi

autoload -U age

alias -g ND='*(/om[1])' # newest directory
alias -g NF='*(.om[1])' # newest file
alias -g L='| less'
alias -g LL='2>&1 | less'
TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
