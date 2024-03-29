#!/usr/bin/env bash
# vim: set ft=bash ts=4 sw=0 tw=0 et:
#
# Print all 256 Xterm color numbers.
#
# To actually set one of these colors in the terminal, output
#     ESC [ 3 8 ; 5 ; <number> m
# Replace "3" with "4" to set the background.
#
# Copyright 2013 Mark Lodato <lodato@google.com>
# Released under the MIT license; see LICENSE for details.

ARGS=$(getopt -n "$0" -o b:f: --long fg:,foreground:,bg:,background: -- "$@")
rc="$?"
[[ $rc -ne 0 ]] && exit $rc
eval set -- "$ARGS"

background=
foreground=
while true; do
    case "$1" in
        -b|--bg|--background)
            background="$2"
            shift
            ;;
        -f|--fg|--foreground)
            foreground="$2"
            shift
            ;;
        --)
            shift
            break
            ;;
    esac
    shift
done

code=3

print_colors() {
    width="$1"
    shift
    while test $# -gt 0; do
        printf '\e['$code'8;5;%dm%'$width'd\e['$code'9m ' "$1" "$1"
        shift
    done
    echo
}

if [[ -n $background && -n $foreground ]]; then
    echo "ERROR: --background and --foreground are mutually exclusive" >&2
    exit 1
fi
if [[ -n $foreground ]]; then
    printf '\e[38;5;%dm' "$foreground"
    code=4
fi
if [[ -n $background ]]; then
    printf '\e[48;5;%dm' "$background"
fi
print_colors 2 {0..7}
print_colors 2 {8..15}
print_colors 3 {16..21} {52..57} {88..93}
print_colors 3 {22..27} {58..63} {94..99}
print_colors 3 {28..33} {64..69} {100..105}
print_colors 3 {34..39} {70..75} {106..111}
print_colors 3 {40..45} {76..81} {112..117}
print_colors 3 {46..51} {82..87} {118..123}
print_colors 3 {124..129} {160..165} {196..201}
print_colors 3 {130..135} {166..171} {202..207}
print_colors 3 {136..141} {172..177} {208..213}
print_colors 3 {142..147} {178..183} {214..219}
print_colors 3 {148..153} {184..189} {220..225}
print_colors 3 {154..159} {190..195} {226..231}
print_colors 3 {232..243}
print_colors 3 {244..255}
printf '\e[0m'
