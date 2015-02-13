#!/bin/sh

# Copyright (C) 2008-2013 Olivier Boudeville
#
# This file is part of the Ceylan Erlang library.


# Implementation notes:

# Previously the specified code was run with 'erl -eval [...]'. This was simple,
# however none of the execution facilities offered by the 'init' module ("-s",
# "-eval" and "-run") allows to run a VM which would resist exceptions (ex: the
# first remote node to crash would trigger a 'noconnection' exception that would
# make the launched node crash).
#
# So we switched to using run_erl, which is more heavyweight but restores the
# resistance to exceptions (ex: relying on pipes).


# Default (0) is to use run_erl (the preferred way, in order to have the VM be
# able to resist to the crash of others; however then some tests may fail while
# either returning success or may terminate in an open shell):
#
#use_run_erl=0
use_run_erl=1


default_node_name="ceylan_default"

# Not used anymore as the user may prefer file-based cookies:
#default_cookie="ceylan"


# Some defaults:

# By default up to 1.2 million processes could be created on one node:
# (reduced, as even without having spawned these processes, the memory footprint
# can increase quite a lot; default value is 32768 processes)
# max_process_count=120000
max_process_count=400000
#max_process_count=120000000


asynch_thread_count=128



USAGE="
Usage: "`basename $0`" [-v] [-c a_cookie] [--sn a_short_node_name | --ln a_long_node_name] [--tcp-range min_port max_port] [--epmd-port new_port] [--fqdn a_fqdn] [--max-process-count max_count] [--busy-limit kb_size] [--async-thread-count thread_count] [--background] [--non-interactive] [--eval an_expression] [--no-auto-start] [-h] [--beam-dir a_path] [--beam-paths path_1 path_2]...: launches the Erlang interpreter with specified settings.

Detailed options:
	-v: be verbose
	-c a_cookie: specify a cookie, otherwise no cookie will be specifically set
	--sn a_short_node_name: specify a short name (ex: 'my_short_name')
	--ln a_long_node_name: specify a long name (ex: 'my_long_name')
	--tcp-range min_port max_port: specify a TCP port range for inter-node communication (useful for firewalling issues)
	--epmd-port new_port: specify a specific EPMD port
	--fqdn a_fqdn: specify the FQDN to be used
	--max-process-count max_count: specify the maximum number of processes per VM (default: ${max_process_count})
	--busy-limit size: specify the distribution buffer busy limit, in kB (default: 1024)
	--async-thread-count thread_count: specify the number of asynchronous threads for driver calls (default: ${asynch_thread_count})
	--background: run the launched interpreter in the background (ideal to run as a daemon, ex: on a server)
	--non-interactive: run the launched interpreter with no shell nor input reading (ideal to run through a job manager, ex: on a cluster)
	--eval 'an Erlang expression': start by evaluating this expression
	--no-auto-start: disables the automatic execution at VM start-up
	-h: display this help
	--beam-dir a_path: adds specified directory to the path searched for beam files (multiple --beam-dir options can be specified)
	--beam-paths first_path second_path ...: adds specified directories to the path searched for beam files (multiple paths can be specified; must be the last option)

Other options will be passed 'as are' to the interpreter Unless --sn or --ln is specified, default is to use a long node name, '${default_node_name}'.
Example: launch-erl.sh -v --ln ceylan --eval 'class_TimeManager_test:run()'"



#echo "Received as parameters: $*"


#ERL=/usr/bin/erl
ERL=`which erl`

RUN_ERL=`which run_erl`
TO_ERL=`which to_erl`


CEYLAN_ERLANG=`dirname $0`/../..
#echo "CEYLAN_ERLANG = ${CEYLAN_ERLANG}"


# If logs are redirected to file:
DEFAULT_LOG_FILE="Ceylan-run.log"

# Defaults:
be_verbose=1
use_tcp_range=1
autostart=0
in_background=1
non_interactive=1

# Erlang defaults (see http://erlang.org/doc/man/erl.html#+zdbbl):
busy_limit=1024




# To stop interpreting options when having to manage them verbatim:
do_stop=1


while [ $# -gt 0 ] && [ $do_stop -eq 1 ] ; do

	token_eaten=1

	#echo "Examining next argument: '$1'"

	if [ "$1" = "-v" ] ; then
		be_verbose=0
		token_eaten=0
	fi

	if [ "$1" = "-c" ] ; then
		shift
		#echo "  + specified cookie: $cookie"
		cookie="$1"
		token_eaten=0
	fi

	if [ "$1" = "--sn" ] ; then
		shift
		short_name="$1"
		token_eaten=0
	fi

	if [ "$1" = "--ln" ] ; then
		shift
		long_name="$1"
		token_eaten=0
	fi

	if [ "$1" = "--tcp-range" ] ; then
		shift
		use_tcp_range=0
		lower_tcp_port="$1"
		higher_tcp_port="$2"
		shift
		# Already done at the end of the loop: shift
		#echo "  + TCP range: from $lower_tcp_port to $higher_tcp_port"
		token_eaten=0
	fi

	if [ "$1" = "--epmd-port" ] ; then

		shift
		epmd_port="$1"
		# This is apparently the way to notify a VM of the EPMD port, and
		# appending the epmd_port_opt before the command apparently will not
		# work ('ERL_EPMD_PORT=4269: not found'), thus exporting it instead:
		#epmd_port_opt="ERL_EPMD_PORT=$epmd_port"
		export ERL_EPMD_PORT=$epmd_port

		# This works both ways (to tell EPMD where to be launched, to tell ERL
		# where to find EPMD).
		token_eaten=0

	fi

	if [ "$1" = "--fqdn" ] ; then
		shift
		fqdn="$1"
		token_eaten=0
	fi


	if [ "$1" = "--max-process-count" ] ; then
		shift
		max_process_count="$1"
		token_eaten=0
	fi


	if [ "$1" = "--busy-limit" ] ; then
		shift
		busy_limit="$1"
		token_eaten=0
	fi


	if [ "$1" = "--async-thread-count" ] ; then
		shift
		asynch_thread_count="$1"
		token_eaten=0
	fi


	if [ "$1" = "--background" ] ; then
		in_background=0
		token_eaten=0
	fi

	if [ "$1" = "--non-interactive" ] ; then
		non_interactive=0
		token_eaten=0
	fi

	if [ "$1" = "--beam-paths" ] ; then
		# Keep --beam-paths if first position, as will be shifted in end of loop
		while [ ! $# -eq 1 ] ; do
			#echo "  + adding beam path $2"
			code_dirs="${code_dirs} $2"
			shift
		done
		token_eaten=0
	fi

	if [ "$1" = "--eval" ] ; then
		shift
		# We can use -s instead, which would allow to send multiple commands
		# in a row.
		to_eval="-eval $1"
		eval_content="$1"
		token_eaten=0
	fi

	if [ "$1" = "--no-auto-start" ] ; then
		#echo "Autostart deactivated"
		autostart=1
		token_eaten=0
	fi

	if [ "$1" = "-h" ] ; then
		echo "$USAGE"
		exit
		token_eaten=0
	fi

	if [ "$1" = "--beam-dir" ] ; then
		shift
		#echo "  + adding beam dir $1"
		code_dirs="${code_dirs} $1"
		token_eaten=0
	fi

	# Ignore options that have to be interpreted by the program itself:
	if [ "$1" = "-start-verbatim-options" ] ; then
		do_stop=0
		shift
		verbatim_opt="${verbatim_opt} $*"
		token_eaten=0
	fi

	if [ "$1" = "--batch" ] ; then
		verbatim_opt="${verbatim_opt} $1"
		token_eaten=0
	fi

	if [ $token_eaten -eq 1 ] ; then
		echo "Warning, unknown argument ('$1'), "
		"adding it 'as is' to command-line." 1>&2
		verbatim_opt="${verbatim_opt} $1"
	fi

	# Prevents an unwanted shift to be done if no verbatim option was specified:
	if [ $do_stop -eq 1 ] ; then
		shift
	fi

done

#echo "Verbatim options: '${verbatim_opt}'."

# The PID of this current UNIX process:
shell_pid=$$

if [ $use_run_erl -eq 0 ] ; then

	# Suffixes the PID, for unicity:
	run_pipe=/tmp/launch-erl-${shell_pid}

	write_pipe="${run_pipe}.w"

	read_pipe="${run_pipe}.r"

	# sh does not have '-p':
	if [ -e "${write_pipe}" ] ; then
		#echo "(removing write pipe)"
		/bin/rm -f "${write_pipe}"

	fi

	if [ -e "${read_pipe}" ] ; then
		#echo "(removing read pipe)"
		/bin/rm -f "${read_pipe}"

	fi


	if [ -e "${write_pipe}" ] ; then
		echo "  Error, write pipe resisted deletion." 1>&2
		exit 50
	fi

	if [ -e "${read_pipe}" ] ; then
		echo "  Error, read pipe resisted deletion." 1>&2
		exit 55
	fi


fi



#+W w : log warnings as warnings.
#log_opt="+W w -kernel error_logger "{file,\"$DEFAULT_LOG_FILE\"}
log_opt="+W w"


# Not using '-smp auto' anymore, as the SMP mode is needed even with a single
# core if GUI (WxWindows) is to be used:
#
# +native not used here:
code_opt="-pz ${code_dirs} -smp +K true +A ${asynch_thread_count} +zdbbl ${busy_limit}"


# Adding the executable last to be able to prefix options:
command="${log_opt} ${code_opt} +P ${max_process_count}"

# Adds a command-line cookie only if specified:
if [ -n "${cookie}" ] ; then
	cookie_opt="-setcookie ${cookie}"
fi


if [ ${autostart} -eq 1 ] ; then
	echo " ** No autostart wanted, but you can run manually: ${eval_content} **"
	to_eval="-eval io:format(\"-->"${eval_content}".\")"
fi


if [ $use_tcp_range -eq 0 ] ; then

	tcp_port_opt="-kernel inet_dist_listen_min ${lower_tcp_port} inet_dist_listen_max ${higher_tcp_port}"

fi

command="${command} ${cookie_opt} ${tcp_port_opt}"



# nslookup could be used as well:
# (some laptops timeout when using the 'host' command)
if [ -z "${fqdn}" ] ; then

	# Not used anymore:
	#fqdn=`host \`hostname\` | awk '{ print $1 }' | head -n 1`
	fqdn=$(hostname -f)
	#echo "Guessed FQDN is ${fqdn}"

fi


# Regardless of short or long:
actual_name=""

if [ -n "${short_name}" ] ; then

	if [ -z "${long_name}" ] ; then

		command="${command} -sname ${short_name}"

		actual_name="${short_name}"

	else

		echo "Error, --sn and --ln cannot be used simultaneously." 1>&2

		exit 1

	fi

	if [ $be_verbose -eq 0 ] ; then

		echo "Launching: ${command}"

	else

		# No-op:
		:

		#echo "Launching the Erlang VM with short name ${short_name}"

	fi

else

	if [ -z "${long_name}" ] ; then

		long_name="${default_node_name}"

	fi

	actual_name="${long_name}"

	# Commented-out, as otherwise we will indeed avoid the "Can't set long node
	# name! Please check your configuration" blocking error, but afterwards
	# Indeed one should let the VM adds by itself the proper hostname:
	#long_name="${long_name}@${fqdn}"

	command="${command} -name ${long_name}"

	if [ $be_verbose -eq 0 ] ; then

		echo "Launching: ${command}"

	else

		# No-op:
		:

		#echo "Launching the Erlang VM with long name ${long_name}"

	fi

fi


if [ $in_background -eq 0 ] ; then

	# -detached used, and implies (among other things like being a
	# non-blocking command), '-noinput -noshell':
	background_opt="-detached"

fi


if [ $non_interactive -eq 0 ] ; then

	# No -detached used here:
	non_interactive_opt="-noinput -noshell"

fi



# Note: the EPMD port cannot be set that way, apparently. It can be set thanks
# to an export, though (see ERL_EPMD_PORT above).
#
#command="$epmd_port_opt ${command} ${background_opt} ${non_interactive_opt} ${verbatim_opt}"


# We removed '--launch-erl-pid=${shell_pid}', not used anymore:
command="${command} ${background_opt} ${non_interactive_opt} ${verbatim_opt}"


if [ $use_run_erl -eq 0 ] ; then

	#echo "Launching a VM, using run_erl."
	final_command="${RUN_ERL} -daemon ${run_pipe} . \"exec ${ERL} ${command}\""

else

	#echo "Launching a VM, using direct command-line execution."
	final_command="${ERL} ${to_eval} ${command}"

fi


# Uncomment to see the actual runtime settings:

# Log to text file:
#echo "$0 running final command: ${final_command}" > launch-erl-command.txt

# Log to console:
#echo "$0 running final command: '${final_command}'"


if [ $use_run_erl -eq 0 ] ; then

	# eval is needed for nested expansion, otherwise:
	# 'Syntax error: Unterminated quoted string'.
	eval "${final_command}"

	# We want to halt the next 'tail -f' when the interpreter stops. For that we
	# have to find its PID.
	#erl_pid=""

	# while [ -z "$erl_pid" ] ; do

	# erl_pid=`ps -edF -w -w | grep beam.smp | grep "launch-erl-pid" | awk '{print $2}'`
	# ps -edf | grep beam.smp
	# echo "erl_pid = $erl_pid"

	# done

	#echo "erl_pid = $erl_pid"

else

	${final_command}

fi


res=$?

# However run_erl may return 0 despite errors:
if [ ! $res -eq 0 ] ; then

	echo "Command failed, with error result $res." 1>&2
	exit $res

fi


#pid=$!

# Commented out, as pid never set:
#if [ $in_background -eq 0 ] ; then
#	echo "(PID of launched interpreter is $pid)"
#fi


if [ $use_run_erl -eq 0 ] && [ $autostart -eq 0 ] ; then

	#echo "Waiting for the creation of write pipe '${write_pipe}'."

	# Wait for creation:
	#

	# Number of seconds before time-out:
	wait_max=60

	wait_count=0

	while [ ! -e "${write_pipe}" ] ; do

		wait_remain=$(($wait_max - $wait_count))

		if [ $wait_remain -eq 0 ] ; then

			echo  "Error, time-out while waiting for the creation of write pipe. Check that there is no identically named Erlang VM running in the background that would block this launch." 1>&2

			# On at least some cases, the name is never found (too long
			# command-line truncated):
			#ps -edf | grep beam | grep "$actual_name" | grep -v grep >&2

			exit 25

		fi

		# Do not start displaying the count-down before 5 seconds, then only
		# every 5 seconds:
		#
		if [ $wait_count -gt 4 ] && [ `expr $wait_count % 5` -eq 0 ] ; then
			echo " (launch time-out in $wait_remain seconds)"
		fi

		sleep 1

		wait_count=$(($wait_count+1))

	done

	#echo "Write pipe '${write_pipe}' found."

	#echo "Evaluating: '${eval_content}.'"

	# Then send the actual command:
	echo "${eval_content}." >> ${write_pipe}

	#echo "Running '${TO_ERL} $run_pipe' now."
	${TO_ERL} $run_pipe
	#echo "(to_erl just finished)"

	# Apparently, with run_erl/to_erl, in some cases some VMs linger, though
	# (i.e. to_erl terminates whereas apparently some VMs like
	# Datalogging_test-boudevil still exist).
	#
	# This is currenly not detected. However, now, a next clashing launch will
	# be clearly advertised (at the start of the second instance, not at the
	# expected end of the first).

	# And monitors its result:

	# Clean-up:

	if [ -e "${write_pipe}" ] ; then

		/bin/rm -f "${write_pipe}"

	fi


	if [ -e "${read_pipe}" ] ; then

		/bin/rm -f "${read_pipe}"

	fi

fi
