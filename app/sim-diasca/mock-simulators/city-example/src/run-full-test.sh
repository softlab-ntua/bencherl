#!/bin/sh

echo "   Running full City-Example benchmarking tests"


make clean all EXECUTION_TARGET=production && for scale in tiny small
medium large huge ; do for duration in short medium long ; do  echo
"     Running benchmarking case with scale $scale and duration
$duration" ; make batch CASE_SCALE=$scale CASE_DURATION=$duration 1>
`date
'+%Y%m%d-%Hh-%Mm-%Ss'`-benchmarking-scale-$scale-duration-$duration.txt
2>&1 ; done ; done && echo "   All tests run."
