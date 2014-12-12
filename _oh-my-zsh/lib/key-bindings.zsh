# -*- mode: sh -*-

# ^ Ctrl
# \eb Alt
# \e Meta

#standard widigets: http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets
#

# Ensures that $terminfo values are valid and updates editor information when keymap changes.
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

#------------------------------------------------------------------------------
# bindkey -e
bindkey -M emacs ' ' magic-space    # also do history expansion on space

bindkey -M emacs '^[[Z' reverse-menu-complete

#------------------------------------------------------------------------------

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^k'    kill-line
bindkey -M viins '^_'    undo
bindkey -M viins '^R'    redo
bindkey -M viins '^w'    vi-backward-kill-word
bindkey -M viins '^f'    vi-forward-word
bindkey -M viins '^b'    vi-backward-word
bindkey -M viins '\eb'   vi-backward-blank-word
bindkey -M viins '\ef'   vi-forward-blank-word
#
# VI MODE KEYBINDINGS (cmd mode)
bindkey -M vicmd 'u'     undo
bindkey -M vicmd '^R'    redo

bindkey -v
