#!/bin/sh

result_dir=`pwd`

plot_extension="png"

gnuplot=`which gnuplot`

USAGE="
Usage: "`basename $0`" [--result-dir RESULT-DIR]

   --result-dir: allows to specify a base Sim-Diasca result directory (as an absolute path), in which the 'simulation-results' and 'performance-monitoring' directories are located

This script allows to generate all result reports from their collected data and command files, should these reports be unavailable. The main purpose of this script is notably to help the user investigating more easily any simulation crash which would have prevented the performance tracker to complete and thus to generate plots out of the already-collected time series.
"


# Generates the plot corresponding to specified command file.
generate_plot_for()
{

	command_filename="$1"

	#echo "$command_filename"

	image_filename=`echo $command_filename|sed "s|\.p$|.$plot_extension|1"`

	if [ -f "$image_filename" ]; then

		echo "   ($image_filename was already generated)"

	else

		echo "   + generating $image_filename"
		$gnuplot $command_filename

		if [ ! $? -eq 0 ] ; then
					# Non-blocking failure:
			echo "Warning: generation of '$image_filename' from '$command_filename' failed." 1>&2
		fi


	fi

}



while [ -n "$*" ] ; do

	token_eaten=1

	if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then

		echo "$USAGE"
		exit

	fi


	if [ "$1" = "--result-dir" ] ; then

		shift
		result_dir=$1
		shift

		token_eaten=0

	fi


	if [ $token_eaten -eq 1 ] ; then

		echo "  Error, unexpected parameters ($*). $USAGE" 1>&2
		exit 4

	fi

done

#echo "result directory: $result_dir"


if [ ! -d "$result_dir" ]; then

		echo "  Error, specified result directory ('$result_dir') does not exist. $USAGE" 1>&2
		exit 6

fi


if [ ! -x "$gnuplot" ] ; then

		echo "  Error, no gnuplot executable found. $USAGE" 1>&2
		exit 7

fi


simulation_result_dir="$result_dir/simulation-results"

performance_track_dir="$result_dir/performance-monitoring"

#echo $simulation_result_dir

echo "
 Performing a post-mortem generation of all results that need it..."

#echo "performance monitoring directory: $performance_track_dir "

if [ -d "$performance_track_dir" ] ; then

	echo "  - taking care of performance monitoring results... "

	cd $performance_track_dir

	for command_filename in *.p; do

		generate_plot_for $command_filename

	done

else

	echo "  Warning: no 'performance-monitoring' directory found." 1>&2

fi


#echo "simulation results directory: $simulation_result_dir"


if [ -d "$simulation_result_dir" ]; then

	cd $simulation_result_dir

	echo "  - taking care of simulation results... "

	# Not the same as '*.p', if no .p file exists:
	target_files=`ls *p 2>/dev/null`

	if [ -n "$target_files" ] ; then

		for command_filename in $target_files; do

			generate_plot_for $command_filename

		done

	else

		echo "  Warning: no plot file found." 1>&2

	fi

else

	echo "  Warning: no 'simulation-results' result directory found." 1>&2

fi

echo " End of post-mortem generation."