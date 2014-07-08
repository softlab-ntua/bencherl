#!/bin/sh

# Defaults section.

# Set to 0 to have detailed debug information:
do_debug=1


# Number of hours before the simulation is killed by the job manager:
max_duration="48"


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
	if [ -z `echo $path| sed 's|^/.*||1'` ] ; then

		# Already absolute:
		absolute_path=$path

	else

		# Absolutizing it:
		absolute_path=`pwd`"/$path"
	fi

}



USAGE="Usage: "`basename $0`" [ -h | --help ] [ -d | --debug ] [ --install-root PATH ] [ --node-count NODE_COUNT ] [ {--cores-per-node,--cpn} CORE_COUNT ] [ --queue QUEUE_NAME] [ --max-duration M ] [ --mail MAIL_ADDRESS EVENT_SPËCIFICATION ] CASE_PATH
 Launches specified Sim-Diasca simulation case on a cluster with specified resource requirements. Ensures first that the Sim-Diasca installation is fully built.
 CASE_PATH is either an absolute path to the case to run, or a path relative to the Sim-Diasca install root.
 Options are:
   --help: displays this message
   --debug: activates the debug mode
   --install-root: specifies the path of the Sim-Diasca installation to use
   --node-count: specifies the number of requested computing nodes
   --cores-per-node or --cpn: specifies how many cores are requested on each node
   --queue: specifies which job queue is to be used (cluster-specific, ex: parall_128)
   --max-duration: specifies the maximal duration, in wall-clock time, as HH (an interger number of hours) or HH:MM:SS (not depending on the number of nodes) for this simulation [default: ${max_duration}h]
   --mail: specifies the email address to which run notifications should be sent; event specification, i.e. 'a' and/or 'b' and/or 'e', must be added afterwards, so that an email is sent respectively when the job is a(borted), b(egun) or e(nded)

Example: "`basename $0`" --install-root /scratch/$USER/my-sim-diasca-install --node-count 4 --cores-per-node 8 --queue parall_1024 --max-duration 24 --mail john.doe@example.org be sim-diasca/src/core/src/scheduling/tests/scheduling_scalability_test.erl"

start_date=`LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S'`


#echo "Usage: $USAGE"


# Note: outputs from this script will be displayed on the console (only).


# Defaults:

# Default path (used in no install could be inferred), often a symbolic link to
# the selected version available in the same directory:
default_install_root="/scratch/$USER/Software/Sim-Diasca/Sim-Diasca-current-install"

install_root=""
case_path=""
node_count=1
core_count=1
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


if [ -n "$mail_notifications" ] ; then

	test_mail=`echo "$mail_notifications"|sed 's|[abe]\{1,3\}|sim-diasca-correct|1'`

	if [ ! "$test_mail" = "sim-diasca-correct" ] ;  then

		echo "  Error, incorrect mail specification ($mail_notifications), expecting a and/or b and/or e." 1>&2
		exit 60

	fi

fi


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
	echo "max_duration       = $max_duration"
	echo "mail               = $mail"
	echo "mail_notifications = $mail_notifications"

	echo

fi



# First, determine Sim-Diasca install root.


# Each clause is to 'cd' to target directory, so that we can display them all as
# absolute directories (with pwd):
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

	echo "  Using specified install root directory $install_root."
	cd $install_root

else

	# Guessing the Sim-Diasca install root from the location of this script:
	install_root=`dirname $0`/../../..

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
		install_root=`pwd`

		echo "  Using the Sim-Diasca install root guessed from the location of this script ("`pwd`")."

	fi

fi


launch_script_name="job-launcher.sh"
launch_script=`PATH=$install_root/sim-diasca/conf/clusters:$PATH which $launch_script_name`

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
first_char=`echo "$case_path"|head -c 1`

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
#actual_max_duration=`expr $max_duration \* 3600 \* $node_count`

# Apparently this is just an overall duration specified in HH:MM:SS; as we can
# specify it just as HH, let's canonicalise it:

if [ ! `echo $max_duration|grep ':'` ] ; then

	actual_max_duration="$max_duration:00:00"

else

	actual_max_duration="$max_duration"

fi


if [ -z "$actual_max_duration" ] ; then

	echo "Error, unable to handle maximum duration ('$max_max_duration')." 1>&2
	exit 55

fi

actual_case_dir=`dirname $actual_case_path`
actual_case_file=`basename $actual_case_path`
actual_case_target=`echo $actual_case_file | sed 's|_test.erl$|_cluster_run|1' | sed 's|_sim.erl$|_cluster_run|1'`

if [ $do_debug -eq 0 ] ; then

	echo "actual_case_dir     = $actual_case_dir"
	echo "actual_case_file    = $actual_case_file"
	echo "actual_case_target  = $actual_case_target"
	echo "actual_case_path    = $actual_case_path"
	echo "actual_max_duration = $actual_max_duration"
	echo "current directory   = $PWD"

fi


# Checkings are over, acting now.

echo "Started on $start_date."

# First, ensures that Sim-Diasca is built: (we are already at the install root)
#
# Note: should a module have to be rebuilt, it would be compiled with default
# settings (i.e. with default EXECUTION_TARGET), which may not be wanted. So the
# best procedure involves compiling the source tree beforehand:
echo "  Ensuring that Sim-Diasca is fully built..."

if [ $do_debug -eq 0 ] ; then

	make all

else

	make all 1>/dev/null

fi


if [ ! $? -eq 0 ] ; then

	echo "  Error, rebuild of the Sim-Diasca installation failed." 1>&2
	exit 30

fi


# Second, generation of the corresponding cluster script:

echo "  Generating the cluster script..."

# Hidden file, should never collider with others:
script_name="/tmp/.sim-diasca-generated-launcher-for-$actual_case_target-by-$USER-on-"`date '+%Y%m%d-%Hh%Mm%Ss'`"-$$.sh"


# Not to have problems with permissions:
if [ -f $script_name ] ; then

	/bin/rm -f $script_name

fi

echo "#!/bin/sh" > $script_name
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
#echo "#PBS -N Sim-Diasca-$actual_case" >> $script_name
echo "#PBS -N Sim-Diasca" >> $script_name

echo >> $script_name
echo "# This script was automatically generated by the "`basename $0`" script on " >> $script_name
echo "# host "`hostname -f`" on $start_date by user $USER." >> $script_name

echo >> $script_name

echo "echo \"Actual execution started on \"\`LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S'\`" >> $script_name
echo "echo \"Hostname = \""\`hostname -f\` >> $script_name
echo "echo \"Work directory = \$PBS_O_WORKDIR\"" >> $script_name
echo "echo \"Current directory = \""\`pwd\` >> $script_name
echo "echo \"Shell = \$SHELL\"" >> $script_name
echo "echo \"Job ID = \$PBS_JOBID\"" >> $script_name
echo "echo \"Node file = \$PBS_NODEFILE\"" >> $script_name
echo "node_list=\`cat \$PBS_NODEFILE | uniq\`" >> $script_name
echo "echo \"Node list = \$node_list\"" >> $script_name

echo "host_candidate_file=\"/tmp/.sim-diasca-host-candidates-for-\$USER-\$PBS_JOBID-\$\$.txt\"" >> $script_name
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

echo "Stopped on "`LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S'`"."

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