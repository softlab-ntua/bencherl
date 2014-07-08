set terminal png size 1000,600
set autoscale
unset log
unset label
unset key
set xtic auto
set ytic auto

set xrange [0:40]
set yrange [0:80]

set grid
set title "Typical expected scalability measurements on a given non-trivial distributed simulation case"

set xlabel "Number of computing hosts processing the simulation"
set ylabel "Overall wall-clock duration of the simulation"

set label "Not able to run\nat all: not enough\nresources" at 1,10

set label "Strict minimum number\n of computing hosts " at 9,32 nopoint
set label "" at 8,30 point pointtype 5

set label "Sweet spot for\nthis simulation" at 10,17 nopoint
set label "" at 12,20 point pointtype 5

set label "Slow degradation as model instances\nare increasingly scattered\nover the network" at 27,37

set arrow from 8,0 to 8,30 nohead lc rgb 'red'

set terminal png
set output "typical-scalability.png"
plot "typical-scalability.dat" using 1:2 with linespoints