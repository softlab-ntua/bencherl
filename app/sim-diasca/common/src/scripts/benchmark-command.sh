#!/bin/sh

# Copyright (C) 2010-2012 Olivier Boudeville
#
# This file is part of the Ceylan Erlang library.


USAGE="
Usage: "`basename $0`" <COMMAND>: returns a mean resource consumption for the specified command."

command="$*"

if [ -z "${command}" ] ; then

	echo "  Error, no command specified. $USAGE" 1>&2
	exit 5

fi

#/usr/bin/time --format "Wall-clock time, in [hours:]minutes:second: %E\nTotal number of CPU-seconds used by the system on behalf of the process (in kernel mode), in seconds.Average total (data+stack+text) memory use of the process, in Kilobytes: %K" ${command}

#/usr/bin/time -f "Wallclock time: %E seconds\nUser time: %Us\nSystem time: %Ss\nMemory: %K KB" ${command} 1>/dev/null

echo "Starting timer"

/usr/bin/time -f "Wallclock time: %E seconds\nUser time: %Us\nSystem time: %Ss" ${command} 1>/dev/null