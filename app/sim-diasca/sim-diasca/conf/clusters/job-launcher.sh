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


# With SLURM we have a typical output like:

# Using specified and validated install root directory
# /scratch/E21850/Software/Sim-Diasca/Sim-Diasca-current-install.
#Started on Wednesday, February 4, 2015, at 17:03:07.
#  Rebuilding selectively Sim-Diasca in production mode...
#(not performing any initial clean-up)
#  Unmuting modules of interest
#  Generating the cluster script...
#  Submitting it to the job manager (slurm)...
#Job 2405075 submitted on Wednesday, February 4, 2015, at 17:03:16, waiting until it is scheduled and run...
#Job queued on Wednesday, February 4, 2015, at 17:03:16, waiting for launch...
#Job started on Wednesday, February 4, 2015, at 17:03:19, waiting for completion...



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

	if [ -x "$sbatch_cmd" ] ; then

		[ $be_quiet -eq 0 ] || echo "sbatch found, hence using SLURM."
		system_type="slurm"

	else

		[ $be_quiet -eq 0 ] || echo "no sbatch found hence not SLURM, testing for PBS compliance."

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

detect_job_manager

exec=$1
[ $be_quiet -eq 0 ] || echo "Requesting execution of the $exec executable..."


# The difficulty here is to retrieve *both* the error code and the standard
# output:
sub_output_file="/tmp/.sim-diasca-submission-by-$USER-on-"$(date '+%Y%m%d-%Hh%Mm%Ss')"-$$.txt"

if [ "$system_type" = "pbs" ] ; then

	sub_command="${qsub_cmd}"

elif [ "$system_type" = "slurm" ] ; then

	sub_command="${sbatch_cmd}"

fi

[ $do_debug -eq 1 ] || echo "Submission command: $sub_command ${all_parameters} 1>$sub_output_file 2>&1"

$sub_command ${all_parameters} 1>$sub_output_file 2>&1
res=$?

if [ ! $res -eq 0 ] ; then

	echo
	echo "Error, job submission with $system_type failed (error code: $res).
Error message was:" 1>&2
	cat $sub_output_file 1>&2
	echo
	exit 15

fi


if [ "$system_type" = "pbs" ] ; then

	# output_file might contain an entry like '1806204.cla11pno'.
	# job_id would be then 1806204:
	#job_id=$( cat $output_file | sed 's|\..*$||1' )

	# Apparently now specifying only the job ID (ex: 2891949 instead of
	# 2891949.cla11pno) will not work anymore, 'qstat -f 2891949' will indeed return
	# 'qstat: Unknown Job Id 2891949.cla13pno'. Specifying the full entry read from
	# file (ex: 2891949.cla11pno) will however work:
	job_id=$( cat $sub_output_file )

	echo "Job identifier is $job_id, its state is:"

	# Full listing of the job information:
	state=$( qstat -f $job_id )

	job_name=$( echo "$state" | grep Job_Name | awk '{printf $3}' )

	echo "    Job_Name  = $job_name"
	echo "$state" | grep Job_Owner
	echo "$state" | grep job_state
	echo "$state" | grep queue

elif [ "$system_type" = "slurm" ] ; then

	# We have all the information we need as environment variables set by SLURM
	# in the environment of the launched script, not in this environment:

	#echo "SLURM_JOB_ID=${SLURM_JOB_ID}"
	#echo "SLURM_JOB_NAME=${SLURM_JOB_NAME}"
	#echo "SLURM_JOB_NODELIST=${SLURM_JOB_NODELIST}"
	#echo "SLURM_JOB_NUM_NODES=${SLURM_JOB_NUM_NODES}"
	#echo "SLURM_SUBMIT_DIR=${SLURM_SUBMIT_DIR}"

	#job_id="${SLURM_JOB_ID}"

	if [ ! -f "$sub_output_file" ] ; then

		echo "  Error, no submission output file found ($sub_output_file)." 1>&2
		exit 34

	fi

	job_id=$( cat $sub_output_file | sed 's|^Submitted batch job ||1' )

	job_name=$( scontrol show jobid ${job_id} | grep ' Name=' | sed 's|^.* Name=||1' )

fi


queue_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )

echo "Job ${job_id} submitted on $queue_time, waiting until it is scheduled and run..."

output_file="$job_name.o$job_id"
error_file="$job_name.e$job_id"


# Table of possible job status values (loosely listed in order of appearance):
#
# Q: Queued (pending)
# A: cAncelled
# R: Running
# F: Failed
# C: Completed


running=1
spotted_as_queued=1
failed_lookups=0
max_failed_lookups=30

while [ $running -eq 1 ] ; do

	if [ "$system_type" = "pbs" ] ; then

		# Either Q (queued) or R (running):
		job_status=$( qstat | grep $job_id | awk '{print $5}' )

	elif [ "$system_type" = "slurm" ] ; then

		job_state=$( scontrol show jobid ${job_id} | grep JobState | sed 's| Reason.*$||1' | sed 's|^   JobState=||1' )

		case $job_state in

			"RUNNING")
				job_status="R"
				;;

			"PENDING")
				job_status="Q"
				;;

			"FAILED")
				job_status="F"
				;;

			"CANCELLED")
				job_status="A"
				;;

			"COMPLETED")
				job_status="C"
				;;

			*)
			   echo "Error, unexpected job state for ${job_id}: $job_state"
			   exit 15
			   ;;

		esac

	fi

	if [ $be_quiet -eq 1 ] ; then

		echo "  - job_status = $job_status"
		echo "  - spotted_as_queued = $spotted_as_queued"
		echo "  - failed_lookups = $failed_lookups"
		echo "  - max_failed_lookups = $max_failed_lookups"

	fi

	if [ "$job_status" = "R" ] ; then

		start_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
		echo "Job started on $start_time, waiting for completion..."
		running=0

	elif [ "$job_status" = "F" ] ; then

		echo "Error, submitted job failed (name: $job_name, id: $job_id)." 1>&2
		exit 56

	elif [ "$job_status" = "A" ] ; then

		echo "Error, submitted job was cancelled - abnormal (name: $job_name, id: $job_id)." 1>&2
		exit 57

	elif [ "$job_status" = "C" ] ; then

		echo "Submitted job (name: $job_name, id: $job_id) reported as successfully completed." 1>&2
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


# PBS will create the output and error files when the job is finished, while
# SLURM may create them immediately and write them over time.


if [ "$system_type" = "pbs" ] ; then

	[ $be_quiet -eq 0 ] || echo "Waiting for output file $output_file in "$(pwd)"..."

	while [ ! -f "$output_file" ] ; do sleep 1 ; done

elif [ "$system_type" = "slurm" ] ; then

	# Possibly never set as running:
	[ $be_quiet -eq 0 ] || echo "Waiting until job ${job_id} is not reported as running..."

	while true ; do

		job_state=$( scontrol show jobid ${job_id} | grep JobState | sed 's| Reason.*$||1' | sed 's|^   JobState=||1' )

		#[ $be_quiet -eq 0 ] ||echo "Job ${job_id} in state $job_state"

		if [ ! "$job_state" = "RUNNING" ] ; then
			break
		fi

	done

	# The writing is not instantaneous either:
	[ $be_quiet -eq 0 ] || echo "Waiting for output file $output_file in "$(pwd)"..."

	while [ ! -f "$output_file" ] ; do sleep 1 ; done

fi


stop_time=$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
echo "Job stopped on $stop_time."
echo

echo "##### Output of job $job_name (ID: $job_id) is (in "$(pwd)"/$output_file):"
cat "$output_file"

#echo "Now waiting for error file..."

while [ ! -f "$error_file" ] ; do sleep 1  ; done

res=$( cat "$error_file" )

if [ -z "$res" ] ; then

  echo "#### No error."

else

  echo "##### Error output for job $job_name (ID: $job_id) is (in "$(pwd)"/$error_file):"

  # Strange output:
  #echo $res

  cat "$error_file"

  echo "Job failed at "$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )

  exit 1

fi


if [ $do_debug -eq 1 ] ; then

	/bin/rm -f ${output_file}

else

	echo "Command file left in ${output_file}."

fi


echo "Job finished at "$( LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )
