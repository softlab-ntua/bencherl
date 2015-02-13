#!/bin/sh


# Ensure EXECUTION_TARGET=development

tracked="truck"
#tracked="cell"

test_script="./test-${tracked}.sh"


trace_file="city_benchmarking_loading_test.traces"

count=10

if [ -n "$1" ] ; then

   count="1"

fi

echo " Running $count times the same simulation"

srm log-*
make && make generate

$test_script 1

mv -f $trace_file 1.traces


for t in $(seq 2 $count) ; do

	$test_script $t
	diff -q log-1.txt log-$t.txt
	mv -f $trace_file $t.traces

done
