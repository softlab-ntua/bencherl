#!/bin/sh


# Of course the extended logging at the level of weather cells must have been
# enabled.
#
# At the start and the end of the simulation, all weather state triplets should
# be the same from one run to another, and the engine and the case shall be
# compiled in production mode.


echo "Run #$1"


A="/tmp/log-$1-tmp.txt"

USER=$USER
HOST=$(hostname)

(
	echo "START"
	make run
	echo "STOP"

 ) | sed "s|(city_benchmarking_loading_run-$USER@$HOST)1> ||1" | tee $A


B="log-$1.txt"

echo "INITIALISATION" > $B
cat $A | grep 'cell start' | sort  > $A.start

cat $A.start >> $B


echo >> $B
echo "RUNNING" >> $B

echo >> $B
echo "ENDING" >> $B
cat $A | grep 'cell stop' | sort  > $A.end

cat $A.end >> $B

echo >> $B
grep 'In the course' $A >> $B

/bin/rm $A.start $A.end
