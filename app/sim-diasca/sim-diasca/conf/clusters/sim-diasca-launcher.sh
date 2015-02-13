#!/bin/sh

# Defaults section.

# Set to 0 to have detailed debug information or, preferably, use the --debug
# command line-option:
#
do_debug=1


# Number of hours before the simulation is killed by the job manager:
max_duration="0"


# Transforms specified path in an absolute one.
#
# If it happens to be relative, considers it is relative to the current
# directory.
#
# Resulting path is assigned to the absolute_path variable.
#
set_as_absolute_path()
{

	path=$1

	# A path is absolute iff it starts with "/"
	if [ -z $( echo $path | sed 's|^/.*||1' ) ] ; then

		# Already absolute:
		absolute_path=$path

	else

		# Absolutizing it:
		absolute_path=$(pwd)"/$path"
	fi

}



# Detects the job manager interface to be used.
#
# This function sets the system_type variable either to "slurm" or to "pbs", and
# respectively sets sbatch/qsub.
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



USAGE="Usage: "$(basename $0)" [ -h | --help ] [ -d | --debug ] [ --install-root PATH ] [ --node-count NODE_COUNT ] [ {--cores-per-node,--cpn} CORE_COUNT ] [ --queue QUEUE_NAME ] [ --qos QOS ] [ --key KEY ] [ --max-duration M ] [ --mail MAIL_ADDRESS EVENT_SPËCIFICATION ] CASE_PATH
 Launches specified Sim-Diasca simulation case on a cluster (running either a PBS or a SLURM job manager) with specified resource requirements. Ensures first that the Sim-Diasca installation is fully built.
 CASE_PATH is either an absolute path to the case to run, or a path relative to the Sim-Diasca install root.
 Options are:
   --help: displays this message
   --debug: activates the debug mode
   --install-root: specifies the path of the Sim-Diasca installation to use
   --node-count: specifies the number of requested computing nodes
   --cores-per-node or --cpn: specifies a minimal number of cores to be requested on each socket
   --queue: specifies which job queue (named partition in SLURM) is to be used (cluster-specific, ex: parall_128)
   --qos: specifies the quality of service to be used for that job (project-specific, ex: release)
   --key: specifies a key in order to enable job launching (cluster-specific)
   --max-duration: specifies the maximal duration, in wall-clock time, as HH (an integer number of hours) or HH:MM:SS (not depending on the number of nodes) for this simulation [default: ${max_duration}h]
   --mail: specifies the email address to which run notifications should be sent; event specification must be added afterward, i.e. on PBS  'a' and/or 'b' and/or 'e', must be added afterwards, so that an email is sent respectively when the job is a(borted), b(egun) or e(nded), while on SLURM it can be set to BEGIN, END, FAIL or ALL

Example: "$(basename $0)" --install-root /scratch/$USER/my-sim-diasca-install --node-count 4 --cores-per-node 8 --queue parall_1024 --max-duration 24 --mail john.doe@example.org be sim-diasca/src/core/src/scheduling/tests/scheduling_scalability_test.erl"

start_date=$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')


#echo "Usage: $USAGE"


# Note: outputs from this script will be displayed on the console (only).


# Defaults:

# Default path (used if no install could be inferred), often a symbolic link to
# the selected version available in the same directory:
default_install_root="/scratch/$USER/Software/Sim-Diasca/Sim-Diasca-current-install"

install_root=""
case_path=""
node_count=1
core_count=""
qos=""
key=""
mail=""
mail_notifications=""
queue_name=""

echo

saved_command_line="Command-line was: $0 $*"


while [ -n "$*" ] ; do

	token_eaten=1


	if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then

		echo "$USAGE"
		exit

	fi


	if [ "$1" = "-d" ] || [ "$1" = "--debug" ] ; then

		shift
		do_debug=0

	fi


	if [ "$1" = "--install-root" ] ; then

		shift
		set_as_absolute_path $1
		install_root="$absolute_path"
		shift
		token_eaten=0

	fi


	if [ "$1" = "--node-count" ] ; then

		shift
		node_count="$1"
		shift
		token_eaten=0

	fi


	if [ "$1" = "--cores-per-node" ] || [ "$1" = "--cpn" ] ; then

		shift
		core_count="$1"
		shift
		token_eaten=0

	fi


	if [ "$1" = "--queue" ] ; then

				shift
				queue_name="$1"
				shift
				token_eaten=0

	fi

	if [ "$1" = "--qos" ] ; then

				shift
				qos="$1"
				shift
				token_eaten=0

	fi

	if [ "$1" = "--key" ] ; then

				shift
				key="$1"
				shift
				token_eaten=0

	fi

	if [ "$1" = "--max-duration" ] ; then

				shift
				max_duration="$1"
				shift
				token_eaten=0

	fi


	if [ "$1" = "--mail" ] ; then

		shift
		mail="$1"
		shift
		mail_notifications="$1"
		shift
		token_eaten=0

	fi

	if [ $token_eaten -eq 1 ] ; then

		# Can be either absolute or relative:
		case_path="$1"
		shift

		if [ -n "$*" ] ; then

			echo "  Error, unexpected parameters ($*) after case path ($case_path).

$USAGE" 1>&2

			exit 5

		fi

	fi

done


# Only relevant for PBS:
#
#if [ -n "$mail_notifications" ] ; then
#
#	test_mail=$(echo "$mail_notifications"|sed 's|[abe]\{1,3\}|sim-diasca-correct|1')
#
#	if [ ! "$test_mail" = "sim-diasca-correct" ] ;  then
#
#		echo "  Error, incorrect mail specification ($mail_notifications), expecting a and/or b and/or e." 1>&2
#		exit 60
#
#	fi
#
#fi


if [ -z "$case_path" ] ; then

	echo "  Error, no simulation case specified (case path lacking).

$USAGE" 1>&2
	exit 10

fi



if [ $do_debug -eq 0 ] ; then

	echo
	echo "$saved_command_line"

	echo "install_root       = $install_root"
	echo "case_path          = $case_path"
	echo "node_count         = $node_count"
	echo "core_count         = $core_count"
	echo "queue_name         = $queue_name"
	echo "qos                = $qos"
	echo "key                = $key"
	echo "max_duration       = $max_duration"
	echo "mail               = $mail"
	echo "mail_notifications = $mail_notifications"

	echo

fi



# First, determine Sim-Diasca install root.


# Each clause is to 'cd' to target directory, so that we can display them all as
# absolute directories (with pwd):
#
if [ -n "$install_root" ] ; then

	# Specified by the user:
	if [ ! -d "$install_root" ] ; then

		echo "Error, user-specified install root ($install_root) does not exist." 1>&2
		exit 25

	fi

	if [ ! -d "$install_root/sim-diasca" ] ; then

		echo "Error, user-specified install root ($install_root) exists, but does not look as a Sim-Diasca root (hint: there should be a 'sim-diasca' directory under this root)." 1>&2
		exit 30

	fi

	echo "  Using specified and validated install root directory $install_root."
	cd $install_root

else

	# Guessing the Sim-Diasca install root from the location of this script:
	install_root=$(dirname $0)/../../..

	if [ ! -d "$install_root/sim-diasca" ] ; then

		# Failure, trying to use default one:

		if [ ! -d "$default_install_root" ] ; then

			echo "Error, no install root specified, not run from a Sim-Diasca tree, and default install root ($default_install_root) does not exist, aborting." 1>&2

			exit 35

		else

			install_root="$default_install_root"
			echo "  Warning: unable to guess Sim-Diasca install root, using default one ($install_root)." 1>&2
			cd $install_root

		fi

	else

		cd $install_root

		# To have a proper absolute path:
		install_root=$(pwd)

		echo "  Using the Sim-Diasca install root guessed from the location of this script ("$(pwd)")."

	fi

fi


launch_script_name="job-launcher.sh"
launch_script=$(PATH=$install_root/sim-diasca/conf/clusters:$PATH which $launch_script_name)

#echo $install_root

if [ ! -f "$launch_script" ] ; then

	echo "Error, launcher script ($launch_script_name) not found." 1>&2
	exit 5

fi


if [ $do_debug -eq 0 ] ; then

	echo "Using launch script $launch_script."

fi


# Second, check the simulation case:

actual_case_path=""

# Is the case an absolute directory?
first_char=$( echo "$case_path" | head -c 1 )

if [ "$first_char" = "/" ] ; then

	# Take this absolute path as is:
	actual_case_path="$case_path"

else

	# Must be relative to the install root:
	actual_case_path="$install_root/$case_path"

fi


if [ ! -f "$actual_case_path" ] ; then

	echo "  Error, simulation case not found ('$actual_case_path').

$USAGE" 1>&2

	exit 20

fi



# Determine duration (total, in seconds):
#actual_max_duration=$(expr $max_duration \* 3600 \* $node_count)

# Apparently this is just an overall duration specified in HH:MM:SS; as we can
# specify it just as HH, let's canonicalise it:

if [ ! $( echo $max_duration | grep ':' ) ] ; then

	actual_max_duration="$max_duration:00:00"

else

	actual_max_duration="$max_duration"

fi


if [ -z "$actual_max_duration" ] ; then

	echo "Error, unable to handle maximum duration ('$max_max_duration')." 1>&2
	exit 55

fi

actual_case_dir=$( dirname $actual_case_path)
actual_case_file=$( basename $actual_case_path )
actual_case_target=$( echo $actual_case_file | sed 's|_test.erl$|_cluster_run|1' | sed 's|_sim.erl$|_cluster_run|1' )

if [ $do_debug -eq 0 ] ; then

	echo "actual_case_dir     = $actual_case_dir"
	echo "actual_case_file    = $actual_case_file"
	echo "actual_case_target  = $actual_case_target"
	echo "actual_case_path    = $actual_case_path"
	echo "actual_max_duration = $actual_max_duration"
	echo "current directory   = $PWD"

fi




# Checkings are over, acting now.
#
# We generate here a self-contained script that will store all information of
# interest (ex: parameters to be used) and be sumbmitted to the job manager of
# the cluster by our job launcher facility.
#
# The job managers mostly require job information to be specified in the script
# (rather than as parameters), hence the script must be generated.

# The information (meta-data) in this script will depend on the type of the job manager.

detect_job_manager




# SLURM section.
#
# As we typically run only one job (one simulation), we could use directly
# 'srun'. However, for more flexibility (in the future we might want to run
# multiple parametric jobs for example), we use 'sbatch' (in which multiple
# 'srun' calls could be specified).
#
# So in the generated script all the first lines dedicated to SLURM will have to
# start with a #SBATCH prefix.



echo "Started on $start_date."

# First, ensures that Sim-Diasca is built: (we are already at the install root)
#
# Note: should a module have to be rebuilt, it would be compiled with default
# settings (i.e. with default EXECUTION_TARGET), which may not be wanted. So the
# best procedure involves compiling the source tree beforehand:
#

BUILD_MODE=production

echo "  Ensuring that Sim-Diasca is fully built in ${BUILD_MODE} mode..."

BUILD_OPT="EXECUTION_TARGET=${BUILD_MODE}"

if [ $do_debug -eq 0 ] ; then

	make all ${BUILD_OPT}

else

	make all ${BUILD_OPT} 1>/dev/null

fi


if [ ! $? -eq 0 ] ; then

	echo "  Error, rebuild of the Sim-Diasca installation failed." 1>&2
	exit 30

fi


# Second, generation of the corresponding cluster script:

echo "  Generating the cluster script..."

# Hidden file, should never collide with others:
script_name="/tmp/.sim-diasca-generated-launcher-for-$actual_case_target-by-$USER-on-"$(date '+%Y%m%d-%Hh%Mm%Ss')"-$$.sh"


# Not to have problems with permissions:
if [ -f $script_name ] ; then

	/bin/rm -f $script_name

fi

echo "#!/bin/sh" > $script_name


# Here we enter in sections that depend on the job manager, and start by setting
# their prefix variables:

# Too long, was truncated:
#job_name="Sim-Diasca-$actual_case"
job_name="Sim-Diasca"

if [ "$system_type" = "pbs" ] ; then

	# Note that PBS support shall be updated and tested (not used for a while).

	if [ -z "$core_count" ] ; then

		core_count=1

	fi

	echo "#PBS -l nodes=$node_count:ppn=$core_count" >> $script_name

	if [ -n "$queue_name" ] ; then

		echo "#PBS -q $queue_name" >> $script_name

	fi

	echo "#PBS -l walltime=$actual_max_duration" >> $script_name

	if [ -n "$mail" ] ; then

		echo "#PBS -M $mail" >> $script_name

		if [ -n "$mail_notifications" ] ; then
			echo "#PBS -m $mail_notifications" >> $script_name
		fi

	fi

	# Too long, was truncated:
	echo "#PBS -N $job_name" >> $script_name


elif [ "$system_type" = "slurm" ] ; then


	# Available options listed in http://slurm.schedmd.com/sbatch.html:

	# We list them here in the same order.

	if [ -n "$core_count" ] ; then
		echo "#SBATCH --cores-per-socket=$core_count" >> $script_name
	fi

	echo "#SBATCH --error=$job_name.e%A" >> $script_name

	# We do not want to share nodes with other running jobs:
	echo "#SBATCH --exclusive" >> $script_name

	echo "#SBATCH --job-name=$job_name" >> $script_name

	echo "#SBATCH --mail-user=$mail" >> $script_name

	echo "#SBATCH --mail-type=$mail_notifications" >> $script_name

	echo "#SBATCH --nodes=$node_count" >> $script_name

	echo "#SBATCH --output=$job_name.o%A" >> $script_name

	echo "#SBATCH --partition=$queue" >> $script_name

	if [ -n "$qos" ] ; then
		echo "#SBATCH --qos=$qos" >> $script_name
	fi

	if [ -n "$actual_max_duration" ] ; then
		echo "#SBATCH --time=$actual_max_duration" >> $script_name
	fi

	# Supposedly safer ("Do not begin execution until all nodes are ready for
	# use"):
	echo "#SBATCH --wail-all-nodes=1" >> $script_name

	if [ -n "$key" ] ; then
		echo "#SBATCH --wckey=$key" >> $script_name
	fi

fi


# Section common to all job managers.

echo >> $script_name
echo "# This script was automatically generated by the "$(basename $0)" script on " >> $script_name
echo "# host "$(hostname -f)" on $start_date by user $USER." >> $script_name

echo >> $script_name

# We record information of interest:

echo "echo \"Actual execution started on \"\$\(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S'\)" >> $script_name
echo "echo \"Hostname: \""\$\(hostname -f\) >> $script_name

if [ "$system_type" = "pbs" ] ; then

	echo "echo \"Work directory: \$PBS_O_WORKDIR\"" >> $script_name
	echo "echo \"Job ID: \$PBS_JOBID\"" >> $script_name

elif [ "$system_type" = "slurm" ] ; then

	echo "echo \"Cluster name (SLURM_CLUSTER_NAME): \$SLURM_CLUSTER_NAME\"" >> $script_name
	echo "echo \"Number of CPUs on each allocated node (SLURM_CPUS_ON_NODE): \$SLURM_CPUS_ON_NODE\"" >> $script_name
	echo "echo \"Job ID (SLURM_JOB_ID): \$SLURM_JOB_ID \"" >> $script_name
	echo "echo \"Job name (SLURM_JOB_NAME): \$SLURM_JOB_NAME\"" >> $script_name
	echo "echo \"Host count (SLURM_JOB_NUM_NODES ): \$SLURM_JOB_NUM_NODES\"" >> $script_name
	echo "echo \"Host list (SLURM_JOB_NODELIST): \$SLURM_JOB_NODELIST\"" >> $script_name
	echo "echo \"Used queue (SLURM_JOB_PARTITION): \$SLURM_JOB_PARTITION\"" >> $script_name
	echo "echo \"Host names (SLURMD_NODENAME): \$SLURMD_NODENAME\"" >> $script_name

	echo "echo \"Invoked from directory (SLURM_SUBMIT_DIR): \$SLURM_SUBMIT_DIR\"" >> $script_name

	#echo "echo \" (): \$\"" >> $script_name

fi


echo "echo \"Current directory = \""\$\(pwd\) >> $script_name
echo "echo \"Shell = \$SHELL\"" >> $script_name



# Here come the "trickiest" part, to generate a Sim-Diasca configuration files from these information:

if [ "$system_type" = "pbs" ] ; then

	echo "echo \"Node file = \$PBS_NODEFILE\"" >> $script_name
	echo "node_list=\$\(cat \$PBS_NODEFILE | uniq\)" >> $script_name
	echo "host_candidate_file=\"/tmp/.sim-diasca-host-candidates-for-\$USER-\$PBS_JOBID-\$\$.txt\"" >> $script_name

elif [ "$system_type" = "slurm" ] ; then

	echo "node_list=\$SLURM_JOB_NODELIST)" >> $script_name
	echo "host_candidate_file=\"/tmp/.sim-diasca-host-candidates-for-\$USER-\$SLURM_JOB_ID-\$\$.txt\"" >> $script_name

fi


echo "echo \"Node list: \$node_list\"" >> $script_name
echo "echo \"Host candidate file: \$host_candidate_file\"" >> $script_name


echo "/bin/rm -f \$host_candidate_file" >> $script_name
echo "for n in \$node_list ; do echo \"\$n.\" >> \$host_candidate_file ; done" >> $script_name
echo "echo \"Content of the Sim-Diasca host file:\"" >> $script_name
echo "cat \$host_candidate_file" >> $script_name
echo "cd $actual_case_dir" >> $script_name
echo "make ${actual_case_target} CMD_LINE_OPT=\"--batch --sim-diasca-host-file \$host_candidate_file\"" >> $script_name

if [ $do_debug -eq 1 ] ; then

	echo "/bin/rm -f \$host_candidate_file" >> $script_name

fi


echo >> $script_name
echo "# End of generated script." >> $script_name

chmod +x $script_name

# Third, execution of that generated script:

echo "  Submitting it to the job manager..."

quiet_opt="--quiet"

if [ $do_debug -eq 0 ] ; then

	quiet_opt="--debug"
	echo "Running now: $launch_script $quiet_opt $script_name"

fi

$launch_script $quiet_opt $script_name

res=$?

echo "Stopped on "$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S' )"."

if [ $do_debug -eq 1 ] ; then

	/bin/rm -f $script_name

else

	echo "Generated script left in $script_name."

fi

if [ $res -eq 0 ] ; then

	echo "Execution succeeded."
	exit 0

else

	echo "Execution failed (exit status: $res)." 1>&2
	exit 100

fi