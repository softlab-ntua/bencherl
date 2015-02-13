#!/bin/sh


# Of course the extended logging at the level of waste trucks must have been
# enabled, and the engine and the case shall be compiled in production mode.

# During the simulations, all truck transitions shall be exactly the same, at
# the same tick. As soon as a tiny computation is not reproducible, the states
# tend to vastly diverge over time.


echo "Run #$1"


A="/tmp/log-$1-tmp.txt"

USER=$USER
HOST=$(hostname)

(
	echo "START"
	make run
	echo "STOP"

 ) | sed 's|(city_benchmarking_loading_run-$USER@$HOST)1> ||1' > $A


B="log-$1.txt"

echo "INITIALISATION" > $B
cat $A | grep 'Truck start' | sort > $A.start

cat $A.start >> $B


echo >> $B
echo "RUNNING" >> $B
cat $A | grep 'Truck\|beginTime' | grep -v 'Truck start:' | grep -v 'Truck end:' | sort >> $B


echo >> $B
echo "ENDING" >> $B
cat $A | grep 'Truck stop' | sort  > $A.end

cat $A.end >> $B

echo >> $B
grep 'In the course' $A >> $B

/bin/rm $A.start $A.end
