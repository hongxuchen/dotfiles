#!/bin/bash
YELLOW="\033[1;33m"
RED="\033[0;31m"
ENDCOLOR="\033[0m"
PROG=$@
start_time=$(date +%s%N)
${PROG}
end_time=$(date +%s%N)
printf "${RED}${PROG}${ENDCOLOR} takes ${YELLOW}$(expr \( $end_time - $start_time \) / 1000000)${ENDCOLOR} ms\n"
