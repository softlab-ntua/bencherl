#!/bin/sh

# Copyright (C) 2010-2012 Olivier Boudeville
#
# This file is part of the Ceylan Erlang library.


USAGE="Usage:  "`basename $0`" [ -node NODE_NAME ] [ -setcookie COOKIE ]: shows the activity of Erlang processes on an Erlang node.
Example: etop.sh -node foobar@baz.org -setcookie 'my cookie'"

if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then

	echo "$USAGE"
	exit

fi

if [ -z "$*" ] ; then

	echo
	echo "(you can also specify a node and/or a cookie, use the --help option for more details)"
	echo

fi


# Ex for testing: erl -name foobar@baz.org

etop_base=`which erl|sed 's|bin/erl$|lib/erlang/lib/observer-|1'`

actual_etop_base=`ls ${etop_base}* -d|tail -n 1`

#echo "actual_etop_base = $actual_etop_base"

etop="${actual_etop_base}/priv/bin/etop"

${etop} -interval 1 -output graphical -lines 20 $*
