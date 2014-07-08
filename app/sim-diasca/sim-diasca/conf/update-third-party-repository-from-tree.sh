#!/bin/sh


USAGE="

 Usage: "`basename $0`" SIM_DIASCA_ROOT THIRD_PARTY_REPOSITORY_TREE

 Updates, from specified input Sim-Diasca root (presumably one of our developing branches), an external, third-party tree (typically a checkout'd GIT branch of a remote repository).

 Example: "`basename $0`" $HOME/Project/Sim-Diasca/sources $HOME/Projects/Foobar/src"


if [ ! $# -eq 2 ] ; then

	echo "Error, exactly two parameters are required.$USAGE" 1>&2
	exit 10

fi


sim_diasca_root="$1"

if [ ! -d "$sim_diasca_root" ] ; then

	echo "Error, input Sim-Diasca root '$sim_diasca_root' is not an existing directory." 1>&2
	exit 15

fi


if [ ! -d "$sim_diasca_root/wooper" ] ; then

	echo " Error, input Sim-Diasca root '$sim_diasca_root' does not look like the root of a suitable check-out (no wooper directory found)." 1>&2
	exit 16

fi



remote_repository="$2"

if [ ! -d "$remote_repository" ] ; then

	echo "Error, third-party repository '$remote_repository' is not an existing directory." 1>&2
	exit 20

fi


if [ ! -d "$remote_repository/wooper" ] ; then

	echo " Error, third-party repository '$remote_repository' does not look like the root of a suitable tree (no wooper directory)." 1>&2
	exit 21

fi


if [ `echo $remote_repository | head -c 1` = "/" ] ; then

	absolute_remote_repository="$remote_repository"

else

	absolute_remote_repository=`pwd`"/$remote_repository"

fi


cd $sim_diasca_root

echo " - preparing a release from $sim_diasca_root"

make prepare-release 1>/dev/null

if [ ! $? -eq 0 ] ; then
	echo " Release preparation failed." 1>&2
	exit 50
fi

release_root=$(make info-release | grep -v SIM_DIASCA_RELEASE_BASENAME | grep SIM_DIASCA_RELEASE_BASE| sed 's|^.*= ||1')
echo " - guessing release root ($release_root)"


echo " - copying release to $remote_repository"
cp -r -f ${release_root}/* $absolute_remote_repository

if [ ! $? -eq 0 ] ; then
	echo " Release preparation failed." 1>&2
	exit 50
fi


echo " - cleaning release temporary directory"
make clean-release 1>/dev/null

date=$(LANG= date '+%A, %B %-e, %Y')

message="Sim-Diasca code update, made on $date."

echo " - one can then issue: cd $remote_repository && git add . && git commit -m \"$message\" && git push"