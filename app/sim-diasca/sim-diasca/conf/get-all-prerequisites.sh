#!/bin/sh

all_distro_types="Debian, ArchLinux"

distro_type="Debian"

usage="Usage: "`basename $0`" [-h|--help] [DISTRO_TYPE]: triggers the installation of all Sim-Diasca prerequisites, by default on a ${distro_type} platform. To be run as root.
If specified, DISTRO_TYPE must be in: [${all_distro_types}].
"

if [ "$1" = "-h" -o "$1" = "--help" ] ; then

	echo "$usage"
	exit

fi


if [ ! -z "$1" ] ; then
	distro_type=$1
fi


if [ !  `id -u` -eq 0 ] ; then

	echo "   Error, this script must be run as root." 1>&2

	exit 5

fi




install_debian()
{

	# From common/conf/install-erlang.sh:
	erlang_packets="g++ make libncurses5-dev openssl libssl-dev     \
   libwxgtk2.8-dev libgl1-mesa-dev libglu1-mesa-dev libpng3"

	# From SimDiasca-installation-instructions-english.rst:
	sim_diasca_packets="bzip2 coreutils build-essential g++         \
   libncurses5-dev openssl libssl-dev libwxgtk2.8-dev               \
   libgl1-mesa-dev libglu1-mesa-dev libpng3 uuidgen                 \
   python-docutils eog evince gcc gnuplot gnuplot-nox gnuplot-x11   \
   gqview graphviz uuid-runtime make mplayer nedit subversion ant   \
   openjdk-7-jdk texlive"

	target_packets="${erlang_packets} ${sim_diasca_packets}"

	echo "  Installing Sim-Diasca prerequisites on Debian..."

	apt-get update && apt-get install ${target_packets}


}


install_arch()
{

	erlang_packets=""

	sim_diasca_packets=""

	target_packets=""

	echo "  Installing on Arch Linux..."

	pacman -Sy ....


}


case "${distro_type}" in

	"Debian") install_debian ;;

	"ArchLinux") install_arch ;;

	*) echo "  Error, unknown distribution: '${distro_type}'." 1>&2 ; exit 15

esac
