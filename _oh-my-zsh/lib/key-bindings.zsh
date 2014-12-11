# -*- mode: sh -*-

#------------------------------------------------------------------------------
# bindkey -e
bindkey -M emacs '\ew' kill-region
bindkey -M emacs -s '\el' "ls\n"
bindkey -M emacs '^r' history-incremental-search-backward
bindkey -M emacs "^[[5~" up-line-or-history # Ctrl-p
bindkey -M emacs "^[[6~" down-line-or-history # Ctrl-n

# make search up and down work, so partially type and hit up/down to find relevant stuff
bindkey -M emacs '^[[A' up-line-or-search
bindkey -M emacs '^[[B' down-line-or-search

bindkey -M emacs "^[[H" beginning-of-line
bindkey -M emacs "^[[1~" beginning-of-line
bindkey -M emacs "^[OH" beginning-of-line
bindkey -M emacs "^[[F"  end-of-line
bindkey -M emacs "^[[4~" end-of-line
bindkey -M emacs "^[OF" end-of-line
bindkey -M emacs ' ' magic-space    # also do history expansion on space

bindkey -M emacs "^[[1;5C" forward-word
bindkey -M emacs "^[[1;5D" backward-word

bindkey -M emacs '^[[Z' reverse-menu-complete

# Make the delete key (or Fn + Delete on Mac) work instead of outputting a ~
bindkey -M emacs '^?' backward-delete-char
bindkey -M emacs "^[[3~" delete-char
bindkey -M emacs "^[3;5~" delete-char
bindkey -M emacs "\e[3~" delete-char

#------------------------------------------------------------------------------

# Ensures that $terminfo values are valid and updates editor information when
# the keymap changes.
function zle-keymap-select zle-line-init zle-line-finish {
    # The terminal must be in application mode when ZLE is active for $terminfo values to be valid.
    if (( ${+terminfo[smkx]} )); then
        printf '%s' ${terminfo[smkx]}
    fi
    if (( ${+terminfo[rmkx]} )); then
        printf '%s' ${terminfo[rmkx]}
    fi

    zle reset-prompt
    zle -R
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

bindkey -v

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^k'    kill-line
bindkey -M viins '^w'    backward-kill-word
bindkey -M viins '^f'    vi-forward-word
bindkey -M viins '^b'    vi-backward-word
bindkey -M viins '\eb'    vi-backward-blank-word
bindkey -M viins '\ef'    vi-forward-blank-word
bindkey -M viins '^_'    undo
bindkey -M viins '^R'    redo
# bindkey -M viins '^f'    vi-forward-blank-word
#
# VI MODE KEYBINDINGS (cmd mode)
# bindkey -M vicmd 'u'     undo
# bindkey -M vicmd '^R'    redo
