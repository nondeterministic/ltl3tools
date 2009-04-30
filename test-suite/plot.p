set output "ltl3-tools-results.ps"
set terminal postscript eps enhanced monochrome 

set xlabel "The different LTL specification patterns"
set ylabel "Total number of states and transitions"
set xrange [0:100]
set yrange [0:1000]
set y2range [0:1000]
set grid
set boxwidth 0.65

plot "gnuplot.dat" using 1:2 title 'Both NBAs together' with boxes lt -1,\
     "gnuplot.dat" using 1:3 title 'Resulting monitor' w boxes fs solid 0.4 border lt -1
