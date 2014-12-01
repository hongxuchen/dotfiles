# -*- mode: sh -*-

# Ensures that $terminfo values are valid and updates editor information when
# the keymap changes.
function zle-keymap-select zle-line-init zle-line-finish {
    # The terminal must be in application mode when ZLE is active for $terminfo
    # values to be valid.
    if (( ${+terminfo[smkx]} )); then
        printf '%s' ${terminfo[smkx]}
    fi
    if (( ${+terminfo[rmkx]} )); then
        printf '%s' ${terminfo[rmkx]}
    fi

    zle reset-prompt
    zle -R
}

# zle -N zle-line-init
# zle -N zle-line-finish
# zle -N zle-keymap-select

bindkey -v

bindkey -a u undo
bindkey -M vicmd '^R' redo
bindkey -M viins '^R' history-incremental-search-backward
