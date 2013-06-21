#!/bin/sh
 
qamode="false"
action=$1
shift
 
case "$action" in
    start|stop)
        ;;
    *)
        echo "Usage: $0 {start|stop} [options]"
        exit 2
        ;;
esac
 
options=$(getopt -o a:qh -l append,qamode,help -- $*)
 
if [ $? != 0 ]; then
    exit 2;
fi
 
set -- $options
 
while true
do
    case $1 in
        -a|--append)
            echo append arg $2
            # $1 is -a | --append now
            # $2 is the argument of -a now
            shift
            ;;
        -q|--qamode)
            qamode="true"
            ;;
        -h|--help)
            echo "help"
            exit 0
            ;;
        *)
            break;
            ;;
    esac
    shift
done
 
if [[ "true" == $qamode ]]; then
    echo "qa mode"
else
    echo "no qa mode"
fi
 
case $action in
    start)
        echo "start process"
        ;;
    stop)
        echo "stop process"
        ;;
esac
