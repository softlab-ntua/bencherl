#!/bin/sh

echo "   Running full City-Example benchmarking tests"


# We suppose here everything else has already been recompiled in production
# mode.

#SCALES="tiny small medium large huge"
SCALES="tiny small medium large"

DURATIONS="short medium long"


make clean all EXECUTION_TARGET=production && for scale in $SCALES ; do for duration in $DURATIONS ; do echo "     Running benchmarking case with scale $scale and duration $duration" ; make batch CASE_SCALE=$scale CASE_DURATION=$duration 1> $(date '+%Y%m%d-%Hh-%Mm-%Ss')-benchmarking-scale-$scale-duration-$duration.txt 2>&1 ; done ; done && echo "   All tests run."
