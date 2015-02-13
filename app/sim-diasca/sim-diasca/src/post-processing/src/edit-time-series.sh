#!/bin/sh

initial_dir=`pwd`

# Retrieves the (absolute) directory in which the post-processing facilities
# are, from the actual path of this script:

base_dir=`dirname $0`

cd $base_dir

make edit_time_series_exec EDIT_PARAMS="--initial-dir $initial_dir $*"
