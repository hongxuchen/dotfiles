# use perl like awk/sed
alias ple='perl -wlne'

# pgs - Perl Global Substitution
# find pattern          = 1st arg
# replace pattern       = 2nd arg
# filename                      = 3rd arg
pgs() { # [find] [replace] [filename]
    perl -i.orig -pe 's/'"$1"'/'"$2"'/g' "$3"
}

# Perl grep, because 'grep -P' is terrible. Lets you work with pipes or files.
prep() { # [pattern] [filename unless STDOUT]
    perl -nle 'print if /'"$1"'/;' $2
}
