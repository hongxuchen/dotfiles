#!/bin/sh
BAT_INFO="/proc/acpi/battery/BAT0/info"
CAPACITY=`cat $BAT_INFO |grep "last full capacity:"|cut -f9 -d\ `
BAT_STATE="/proc/acpi/battery/BAT0/state"
LEVEL=`cat $BAT_STATE | grep remaining |cut -f8 -d\ `
echo "$LEVEL*100/$CAPACITY"|bc  
