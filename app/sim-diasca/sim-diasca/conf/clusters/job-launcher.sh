#!/bin/sh

# Defaults:
do_debug=1
be_quiet=1

USAGE="

Usage: "$(basename $0)" [ --quiet ] [ --debug ] EXECUTABLE [PARAMETERS]
Launches the specified executable with specified parameters on the cluster, through either a PBS-based job manager or a SLURM one."

# Note: this script interacts with the job manager, and is not Sim-Diasca
# specific per se.


# This script typically outputs on the console something like (with a
# PBS-compliant job manager):

# Job identifier is 1806204, its state is:
#	Job_Name  = Sim-Diasca
#	Job_Owner = boudevil@cla11pfr
#	job_state = Q
#	queue = parall_8
# Job queued on Thursday, October 21, 2010, at 10:51:41, waiting until it is
# scheduled and run...

# And produces for example following files in
# /scratch/$USER/Software/Sim-Diasca/Sim-Diasca-current-install:
#  - Sim-Diasca.e1806204
#  - Sim-Diasca.e1806204

# A temporary file (ex: /tmp/.sim-diasca-qsub-10733.txt) is also created, it
# just contains the corresponding job ID and server node.



# Detects the job manager interface to be used.
#
# This function sets the system_type variable either to "slurm" or to "pbs", and
# respectively sets sbatch/qsub.
#
# (verbatim copy from the same function defined in sim-diasca-launcher.sh)
#
detect_job_manager()
{

	# We test first specifically for SLURM (via 'sbatch'), as SLURM installs
	# also a (pseudo) 'qsub', whose presence is thus not conclusive:

	system_type="undefined"

	sbatch_cmd=$(which sbatch)

	if [ -x "$salloc_cmd" ] ; then

		[ $be_quiet -eq 0 ] || echo "salloc found, hence using SLURM."
		system_type="slurm"

	else

		[ $be_quiet -eq 0 ] || echo "no salloc found hence not SLURM, testing for PBS compliance."

		qsub_cmd=$(which qsub)

		if [ -x "$qsub_cmd" ] ; then

			[ $be_quiet -eq 0 ] || echo "qsub found, hence using PBS."
			system_type="pbs"

		else

			echo "  Error, no job manager support detected (neither SLURM nor PBS)." 1>&2
			exit 35

		fi

	fi

}


all_parameters="$*"

if [ -z "$all_parameters" ] ; then

	echo "Error, the executable to submit must be specified. $USAGE" 1>&2
	exit 10

fi


is_over=1

while [ $is_over -eq 1 ] ; do

	token_eaten=1


	if [ "$1" = "--quiet" ] ; then

		be_quiet=0
		shift

	fi


	if [ "$1" = "--debug" ] ; then

		do_debug=0
		shift

	fi


	if [ $token_eaten -eq 1 ] ; then

		all_parameters="$*"
		is_over=0

	fi

done


# First we select which tool for job submission should be used:



exec=$1
[ $be_quiet -eq 0 ] || echo "Requesting execution of the $exec executable..."


# The difficulty here is to retrieve *both* the error code and the standard
# output:
sub_output_file="/tmp/.sim-diasca-submission-by-$USER-on-"$(date '+%Y%m%d-%Hh%Mm%Ss')"-$$.txt"

if [ "$system_type" = "pbs" ] ; then

	sub_command="$qsub_cmd ${all_parameters} 1>$sub_output_file 2>&1"

elif [ "$system_type" = "slurm" ] ; then

	sub_command="$salloc_cmd ${all_parameters} 1>$sub_output_file 2>&1"

fi

[ $do_debug -eq 1 ] || echo "Submission command: $qsub_cmd ${all_parameters} 1>$qsub_output_file 2>&1"

$qsub_cmd ${all_parameters} 1>$qsub_output_file 2>&1
res=$?

if [ ! $res -eq 0 ] ; then

	echo
	echo "Error, job submission (qsub) failed (error code: $res).
Error message was:" 1>&2
	cat $qsub_output_file 1>&2
	echo
	exit 15

fi


# qsub_output_file might contain an entry like '1806204.cla11pno'.
# job_id would be then 1806204:
#job_id=$( cat $qsub_output_file | sed 's|\..*$||1' )

# Apparently now specifying only the job ID (ex: 2891949 instead of
# 2891949.cla11pno) will not work anymore, 'qstat -f 2891949' will indeed return
# 'qstat: Unknown Job Id 2891949.cla13pno'. Specifying the full entry read from
# file (ex: 2891949.cla11pno) will however work:
job_id=$( cat $qsub_output_file )


echo "Job identifier is $job_id, its state is:"

# Full listing of the job information:
state=$( qstat -f $job_id )

job_name=$( echo "$state"|grep Job_Name|awk '{printf $3}' )

echo "    Job_Name  = $job_name"
echo "$state" | grep Job_Owner
echo "$state" | grep job_state
echo "$state" | grep queue

queue_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )

echo "Job submitted on $queue_time, waiting until it is scheduled and run..."

output_file="$job_name.o$job_id"
error_file="$job_name.e$job_id"

running=1
spotted_as_queued=1
failed_lookups=0
max_failed_lookups=30

while [ $running -eq 1 ] ; do

	# Either Q (queued) or R (running):
	job_status=$( qstat | grep $job_id | awk '{print $5}' )

	echo "  - job_status = $job_status"
	echo "  - spotted_as_queued = $spotted_as_queued"
	echo "  - failed_lookups = $failed_lookups"
	echo "  - max_failed_lookups = $max_failed_lookups"

	if [ "$job_status" = "R" ] ; then

		start_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
		echo "Job started on $start_time, waiting for completion..."
		running=0

	elif [ "$job_status" = "Q" ] ; then

		# Queued:
		if [ $spotted_as_queued -eq 1 ] ; then

			queue_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
			echo "Job queued on $queue_time, waiting for launch..."
			spotted_as_queued=0

			# Should it disappear again, it should be because it has run:
			failed_lookups=0
			max_failed_lookups=0

		fi

		sleep 1

	else

		if [ -z "$job_status" ] ; then

			if [ $spotted_as_queued -eq 0 ] ; then

				echo "   (job named '$job_name' with id $job_id not found anymore once queued, supposing it was run and completed in the meantime)"

				running=0

			else

				if [ $failed_lookups -eq $max_failed_lookups ] ; then

					echo "Error, submitted job (name: $job_name, id: $job_id) not found even after $failed_lookups look-ups." 1>&2
					echo "Maybe the job run for too short for that script to spot it?" 1>&2

					# We suppose that the job was recorded instantaneously (no
					# race condition between the submission and the query) but
					# finished without having being spotted by this script.
					#
					# Thus there is not point in waiting for a job that most
					# probably already run (was either very short or crash at
					# start-up).
					exit 20

				else

					let failed_lookups="failed_lookups+1"

					# So that the next look-ups have more chance to succeed,
					# should there be a race condition before qsub submission
					# and qstat update.
					echo "   (look-up #${failed_lookups} failed: job named '$job_name' with id $job_id not found yet)"
					sleep 10

				fi

			fi

		else

			echo "Error, unexpected status ($job_status) for job (name: $job_name, id: $job_id), aborting." 1>&2

			exit 55

		fi

	fi

done

#echo
[ $be_quiet -eq 0 ] || echo "Waiting for output file $output_file in "$(pwd)"..."

echo

while [ ! -f "$output_file" ] ; do sleep 1  ; done

stop_time=$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
echo "Job stopped on $stop_time."
echo

echo "##### Output of job $job_name:$job_id is:"
cat $output_file

#echo "Now waiting for error file..."

while [ ! -f "$error_file" ] ; do  sleep 1  ; done

res=$( cat "$error_file" )

if [ -z "$res" ] ; then

  echo "#### No error."

else

  echo "##### Error of job $job_name:$job_id is:"
  echo $res
  exit 1

fi


if [ $do_debug -eq 1 ] ; then

	/bin/rm -f ${qsub_output_file}

else

	echo "Qsub command file left in ${qsub_output_file}."

fi


echo "Job finished."
