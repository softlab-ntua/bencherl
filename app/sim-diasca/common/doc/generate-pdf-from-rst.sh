#!/bin/sh

# Copyright (C) 2009-2012 Olivier Boudeville
#
# This file is part of the Ceylan Erlang library.


USAGE="  Usage: "`basename $0`" <RST FILE>
  Generates a PDF file from the specified RST file, overwriting any past file with that name.
  Ex: '"`basename $0`" my_file.rst' will attempt to generate a new 'my_file.pdf' file."


if [ "$1" = "-h" ] || [ -z "$1" ] ; then

	echo "$USAGE"
	exit
fi


source_file="$1"

if [ ! -f "${source_file}" ] ; then

	echo "  Error, source file ${source_file} not found." 1>&2
	exit 10

fi


#rule_file="$CEYLAN_SRC/doc/GNUmakerules-docutils.inc"
rule_file=`dirname $0`/GNUmakerules-docutils.inc

#echo "rule_file = ${rule_file}"

if [ ! -f "${rule_file}" ] ; then

	echo "  Error, rule file to generate PDF ${rule_file} not found." 1>&2
	exit 11

fi


file_prefix=`echo ${source_file}|sed 's|.rst$||1'`

target_file="${file_prefix}.pdf"

if [ -f "${target_file}" ] ; then

	echo "(removing pre-existing ${target_file})"
	/bin/rm -f "${target_file}"

fi


echo "Generating now ${target_file} from ${source_file}..." && make -f "${rule_file}" "${target_file}" && echo "Generation succeeded!"

/bin/rm -f ${file_prefix}.aux ${file_prefix}.tex ${file_prefix}.out ${file_prefix}.log
