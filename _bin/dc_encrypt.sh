#!/bin/bash

# TODO: use python script

script_name=$(basename $0) #script name
usage () {
    echo
    echo "USAGE: $script_name your_words"
    echo
    echo
    echo "Example:"
    echo "        $script_name example@example.com"
    echo
    echo
    exit 1
}

if [[ -z "$@" ]]
then
    usage
else

    words="$@"
fi

declare -a num_array
num_array=(`echo $words | od -An -t dC | tr -d "\n"`)
num_array=(`echo $words | od -An -t dC `)

answer=$(echo ${num_array[0]}*${num_array[2]} | bc)

length=${#num_array[@]}

tmp=1
answer=0

for(( i=$length-1;i>=0;i-- ))
do
    tmp=${num_array[$i]}
    for(( j=$length-1-$i;j>0;j-- ))
    do
        tmp=$(echo $tmp*256|bc)
        tmp=$(echo $tmp|tr -d "\\\\\ ")
    done
    answer=$(echo $answer+$tmp | bc)
    answer=$(echo $answer|tr -d "\\\\\ ")

done

echo $answer
