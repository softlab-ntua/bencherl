#!/bin/sh


USAGE="

 Usage: "`basename $0`" SIM_DIASCA_ARCHIVE THIRD_PARTY_REPOSITORY_ROOT

 Updates an external, third-party SVN repository from the archive of a Sim-Diasca release.

 Example: "`basename $0`" Sim-Diasca-2.0.11.tar.bz2 $HOME/Projects/Foobar"


# Other example: update-third-party-repository.sh Sim-Diasca-2.0.11.tar.bz2 /home/E21850/Projects/CLEVER/Sources/clever/branches/pre-release-0.1.6


if [ ! $# -eq 2 ] ; then

	echo "Error, exactly two parameters are required.$USAGE" 1>&2
	exit 10

fi


sim_diasca_archive="$1"

if [ ! -f "$sim_diasca_archive" ] ; then

	echo "Error, Sim-Diasca archive $sim_diasca_archive is not an existing file." 1>&2
	exit 15

fi


remote_repository="$2"


if [ ! -d "$remote_repository" ] ; then

	echo "Error, third-party repository $remote_repository is not an existing directory." 1>&2
	exit 20

fi



if [ ! -d "$remote_repository/wooper" ] ; then

	echo " Error, third-party repository $remote_repository does not look like the root of a suitable check-out (no wooper directory)." 1>&2
	exit 25

fi


if [ `echo $sim_diasca_archive | head -c 1` = "/" ] ; then

	absolute_sim_diasca_archive="$sim_diasca_archive"

else

	absolute_sim_diasca_archive=`pwd`"/$sim_diasca_archive"

fi



if [ `echo $remote_repository | head -c 1` = "/" ] ; then

	absolute_remote_repository="$remote_repository"

else

	absolute_remote_repository=`pwd`"/$remote_repository"

fi


echo


# Let's prepare the archive:

tmp_dir="tmp-"`basename $0`

if [ -d "$tmp_dir" ] ; then

	echo "(removing temporary directory $tmp_dir first)"
	/bin/rm -rf "$tmp_dir"

fi


echo " + cleaning target repository (supposedly already updated)"

initial_dir=`pwd`

cd $absolute_remote_repository

make clean 1>/dev/null

if [ ! $? -eq 0 ] ; then

	echo "Error, cleaning failed." 1>&2
	exit 28

fi

cd "$initial_dir"


echo " + extracting Sim-Diasca archive"

mkdir "$tmp_dir"

cd "$tmp_dir"

tar xjf $absolute_sim_diasca_archive

archive_root=`basename $sim_diasca_archive| sed 's|\.tar\.bz2$||1'`

#echo "archive_root = $archive_root"

if [ ! -d "$archive_root" ] ; then

	echo "Error, expected root directory $archive_root not found in archive." 1>&2
	exit 30

fi

cd "$archive_root"

echo " + updating third-party repository"

# We do not want to overwrite the root GNUmakefile:

updated_packages="common wooper traces sim-diasca mock-simulators"

for p in $updated_packages ; do

	echo "   - updating package $p"

	find $p -type d -exec /bin/mkdir -p "$remote_repository/"'{}' ';' 1>/dev/null

	if [ ! $? -eq 0 ] ; then

		echo "Error, directory mirroring failed." 1>&2

		exit 35

	fi


	find $p -type f -exec /bin/cp -f '{}' "$remote_repository/"'{}' ';' 1>/dev/null

	if [ ! $? -eq 0 ] ; then

		echo "Error, file mirroring failed." 1>&2

		exit 40

	fi


done


/bin/rm -rf "$tmp_dir"

echo "
Newer repository status is:"

cd "$remote_repository"

svn status

echo "
 + rebuilding third-party repository from scratch"

make clean all 1>/dev/null

if [ ! $? -eq 0 ] ; then

	echo "Error, rebuilding of third-party repository failed:" 1>&2

	make all

	exit 50

fi

echo "
Repository $remote_repository successfully updated and rebuilt!"


exit 0
