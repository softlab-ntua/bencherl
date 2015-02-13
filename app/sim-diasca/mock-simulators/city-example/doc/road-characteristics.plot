# Gnuplot file, created on Thursday, March 7, 2013.
set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title "Average speed on a road depending on its vehicle load"
set xlabel "Load of the road (in percents)"
set ylabel "Average speed (in kilometers per hour) of an entering vehicle"

set xrange [0:100]

v_min=5
v_max=110

# Coefficient, the higher the slower the speed decreases with load:
alpha=45


set terminal png size 800,600
set output 'road-characteristics.png'


plot v_min + ( v_max - v_min ) * exp( -x / alpha )
