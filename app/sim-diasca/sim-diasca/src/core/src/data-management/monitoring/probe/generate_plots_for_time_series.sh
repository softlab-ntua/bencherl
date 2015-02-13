#!/bin/sh


# This is a simple standalone script to generate plots corresponding to
# time-series, when data_only was used instead of data_and_plot.

# See also generate_plots_for_time_series.erl, for an Erlang-based parallel
# version of it.


dir_prefix="$1"

for d in ${dir_prefix}-* ; do

	if [ -d "$d" ] ; then

		echo "#### Generating plots in $d"

		cd $d

		for f in *.dat ; do

			echo "  - generating plot for $f "
			gnuplot `echo $f | sed 's|.dat|.p|1'`

		done

		cd ..

	fi

done