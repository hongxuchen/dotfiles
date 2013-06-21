#!/usr/bin/gnuplot
# cd to git checkout repository
# git log --author=HongxuChen --date=short | grep ^Date: | cut  -c 9- | uniq -c | cut -c 7- > commit_log.txt
# gnuplot commit_log.plot
# Browse 'commit_log.jpg'
# AUTHOR: qiyaoltc@gmail.com

set grid
set terminal jpeg
set output "commit_log.jpg"

set xdata time;
set timefmt "%Y-%m-%d";
set format x "%Y %m";
set xtics rotate;
set ytics 20

set yrange [0:380]

plot 'commit_log.txt' u 2:1 smooth cumulative t "commit history" lw 2 lc rgb "royalblue"
