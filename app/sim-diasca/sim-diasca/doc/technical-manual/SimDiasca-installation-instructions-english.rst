:raw-latex:`\pagebreak`

-----------------------------
Sim-Diasca Installation Guide
-----------------------------


Objective & Context
===================

The goal is to set up a fully functional Sim-Diasca installation, and to be able to test it and to develop with it, for example so that the implementation of new simulation cases and models can be directly experimented.

We suppose here that:

 - either no GIT access to the Sim-Diasca repository is available: in that case the installation will be performed from an archive instead, supposedly already available (ex: ``Sim-Diasca-x.y.tar.bz2``)

 - or Sim-Diasca is to be installed from its project GIT repository, which allows to update it and send back changes quite easily

The operating system is supposed here to be GNU/Linux [#]_ (32 or 64 bits):

:raw-html:`<img src="xkcd-cautionary.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-cautionary.png}`


.. [#] This is the platform we use routinely, although other UNIX systems and possibly Windows platforms *could* be targeted. Sim-Diasca is also very close to be able to run on MacOS X.

Root access is not necessary, but recommended, so that any lacking prerequisite can be installed with little effort:

:raw-html:`<img src="xkcd-sandwich.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-sandwich.png}`


Some space on disk will be needed. 150 megabytes should be enough for the full Sim-Diasca install by itself, but, depending on your use, generated data (ex: frames for simulation videos, plots of simulation results) could need *a lot* more additional space.

This installation procedure is quite detailed and takes into account different cases. Applying it should not be too difficult or time-consuming, and anyway it just needs to be done once. Upgrading the Sim-Diasca version is then straightforward.

Some support is generally available by email (olivier.boudeville@edf.fr), should an issue be encountered.




Prerequisites
=============


We preferred to list below the widest possible range of tools here. This includes:

 - the software the simulator itself is composed of (ex: the ``Erlang`` runtime, the various intermediate layers)

 - the third-party tools that the simulator may trigger for its own purpose (ex: ``LogMX`` to monitor and browse simulation traces, ``gnuplot`` to render plots - one should ensure that its PNG support is enabled, possibly thanks to ``libgd`` - or ``graphviz`` to render graphs)

 - the other tools that can be used to post-process or make use of the simulation results (ex: ``mplayer``, to display generated videos)

 - finally, the toolchain that can be used to *build* the simulator and its dependencies (ex: to recreate the Sim-Diasca trace parser based on ``LogMX``)



Software Needed
---------------

Software prerequisites for a given host (computer) depend on the role of this host.

One must indeed distinguish here between two kinds of nodes (in the sense of Erlang) involved in a Sim-Diasca simulation:

 - the (single) *user* node, from which the simulation is launched and run, and to which results are retrieved and potentially displayed; there must be exactly one user node per simulation

 - the (potentially numerous) *computing* nodes, which are in charge of the evaluation of all model instances and provide the resources needed by a parallel and distributed simulation; there must be at least one computing node per simulation (which is by default created on the same host as the user node)


These nodes are mapped to actual networked hosts (workstations, computers in a cluster, etc.). The current approach is to have exactly one computing node on each computing host (thus federating all local processors and cores), and have a user node which may or may not be on the same host as a computing node.

So, typically, should just one computer be available, it will then run two nodes (the user node and one computing node). Should a simulation be distributed over three hosts, by default the user host will run a user node and a computing node, whereas the two other hosts will each run one computing node.



Tool List For the Computing Nodes
.................................

An host that is to run only a computing node needs only to have a recent enough Erlang environment installed. As it is a subset of the needs of a user node, please refer to the next section.



Tool List For the User Node
...........................

On the targeted host the specific tools listed below and tagged ``Mandatory`` have to be available: if not readily available, the corresponding packages [#]_ ought to be installed. Some hints about each tool are specified below the table. Afterwards, all installation procedures that are not trivial are described.


.. [#] The package names are the ones used by Debian-based distributions, including Ubuntu. Other distributions might use (often slightly) different names.




+--------------+---------------------+-----------------+----------------------------------------------+
| Tool Name    | Corresponding       | Tool Necessity  | Purpose                                      |
|              | Debian Packages     |                 |                                              |
+==============+=====================+=================+==============================================+
| Erlang       | ``erlang``          | Mandatory       | To generate and run Sim-Diasca. Installing   |
|              |                     |                 | Erlang from sources is strongly recommended  |
|              |                     |                 | (see below).                                 |
+--------------+---------------------+-----------------+----------------------------------------------+
| WOOPER       | (none)              | Mandatory       | Needed by Sim-Diasca. WOOPER sources are     |
|              |                     |                 | already included in the Sim-Diasca ones, so  |
|              |                     |                 | nothing special is to be done for WOOPER.    |
+--------------+---------------------+-----------------+----------------------------------------------+
| LogMX        | (none)              | Recommended     | To monitor the simulation traces (this is the|
|              |                     |                 | default trace supervision tool).             |
+--------------+---------------------+-----------------+----------------------------------------------+
| Gnuplot      | ``gnuplot``,        | Mandatory       | To generate plots of numerical data.         |
|              | ``gnuplot-nox``     |                 |                                              |
|              | and                 |                 |                                              |
|              | ``gnuplot-x11``     |                 |                                              |
+--------------+---------------------+-----------------+----------------------------------------------+
| Dot          | ``graphviz``        | Recommended     | To generate graph renderings.                |
+--------------+---------------------+-----------------+----------------------------------------------+
| GNU make     | ``make``            | Mandatory       | To build and use Sim-Diasca.                 |
+--------------+---------------------+-----------------+----------------------------------------------+
| Mplayer /    | ``mplayer``         | Optional        | To encode and display generated videos.      |
| Mencode      |                     |                 |                                              |
+--------------+---------------------+-----------------+----------------------------------------------+
| Geeqie (was  | ``geeqie`` (formerly| Mandatory       | To browse plots of time series and inspect   |
| Gqview)      | ``gqview``)         |                 | generated frames.                            |
+--------------+---------------------+-----------------+----------------------------------------------+
| Eye of Gnome | ``eog``             | Optional        | To display generated images.                 |
+--------------+---------------------+-----------------+----------------------------------------------+
| Nedit        | ``nedit``           | Optional        | To edit Sim-Diasca sources with a dedicated  |
|              |                     |                 | WOOPER-aware syntax highlighting             |
+--------------+---------------------+-----------------+----------------------------------------------+
| Emacs        | ``emacs``           | Optional        | To edit Sim-Diasca sources.                  |
+--------------+---------------------+-----------------+----------------------------------------------+
| GIT          | ``git``             | Optional        | To be able to read and modify Sim-Diasca     |
|              |                     |                 | sources (note: no access to repository       |
|              |                     |                 | available from outside EDF).                 |
+--------------+---------------------+-----------------+----------------------------------------------+
| GCC          | ``gcc``             | Recommended     | To build Erlang from sources.                |
+--------------+---------------------+-----------------+----------------------------------------------+
| Ant          | ``ant``             | Optional        | To build the Sim-Diasca Java Trace Parser    |
|              |                     |                 | for LogMX (if ever needed).                  |
+--------------+---------------------+-----------------+----------------------------------------------+
| Java (Sun    | ``sun-java6-jdk``   | Optional        | To be able to compile and run a              |
| version or   | or                  |                 | Sim-Diasca-enabled LogMX parser.             |
| OpenJDK)     | ``openjdk-7-jre``   |                 |                                              |
+--------------+---------------------+-----------------+----------------------------------------------+
| Docutils     |``python-docutils``  | Optional        | To generate documentation files from RST.    |
+--------------+---------------------+-----------------+----------------------------------------------+
| LateX and al | ``texlive``         | Optional        | To generate PDF documentation.               |
+--------------+---------------------+-----------------+----------------------------------------------+
| Evince       | ``evince``          | Optional        | To display PDF files.                        |
+--------------+---------------------+-----------------+----------------------------------------------+



Package Walk-Through
....................

Erlang and Sim-Diasca (including its ``Common``, ``WOOPER`` and ``Traces`` layers) are of course needed and their installation is detailed in the next sections.

The use of a custom-built ``Erlang`` environment is strongly recommended, to be able to rely on an adequately-configured and up-to-date version. Thus no need to install any distribution-specific prebuilt Erlang package [#]_, however the ``GCC`` compiler must be available so that the build of Erlang can be performed.

.. [#] It is even safer not to have any system installation of Erlang, so that no version mismatch can ever happen, despite potential mistakes in user shell settings (note that a runtime checking of all the actual Erlang versions in use by each distributed host is performed).


An image viewer, a PDF viewer and a video player are generally useful to browse Sim-Diasca outputs. Defaults are, respectively: *Geeqie* (``geeqie``, previously known as ``gqview``), *Evince* (``evince``), and *Mplayer* (``mplayer``), but they can be replaced very easily, in Sim-Diasca code, by any counterparts that would be preferred [#]_.


.. [#] For that refer to the "default tool section" in ``common/src/utils/executable_utils.erl``.

Depending on the version of the ``Java Runtime Environment`` (JRE) which is available on the target host, the prebuilt Sim-Diasca trace parser may be directly used, otherwise it will have to be rebuilt from its sources. In this latter case only, ``Ant`` and a ``Java SDK`` will be needed.


``Docutils``, ``LateX`` and al are only useful if wanting to be able to generate the documentation of Sim-Diasca, or PDF trace reports.

GIT is needed only if using an internal repository to retrieve the Sim-Diasca sources. Usually sources come from a released archive instead.


.. Note::
	If some actual development, *on* Sim-Diasca or *with* Sim-Diasca, was to be performed (beyond mere testing), then relying on a GIT clone rather than on a source archive should be preferred, so that Sim-Diasca can be updated accordingly (and in both directions) with no effort.


Using ``Nedit`` is a matter of taste, any text editor would be suitable (including ``emacs`` or full IDE like `Erlide <http://erlide.sourceforge.net/index.html>`_, based on `Eclipse <http://www.eclipse.org/>`_), the advantage being just here that an Erlang WOOPER-enabled `syntax highlighting mode <https://sourceforge.net/p/ceylan/common/ci/master/tree/conf/nedit.rc>`_ is available.


Finally, users of Debian-based distributions could just start with the following command to install the main prerequisite packages::

 $ sudo apt-get install bzip2 coreutils build-essential g++         \
   libncurses5-dev openssl libssl-dev libwxgtk2.8-dev               \
   libgl1-mesa-dev libglu1-mesa-dev libpng3 uuidgen                 \
   python-docutils eog evince gcc gnuplot gnuplot-nox gnuplot-x11   \
   gqview graphviz uuid-runtime make mplayer nedit subversion ant   \
   openjdk-7-jdk texlive




Preparing the Sim-Diasca sources
--------------------------------

The sources of Sim-Diasca can be obtained either from an archive file or from the project Subversion repository. Both cases are detailed below.



Installation From a Sim-Diasca Archive
......................................

You should have been given a Sim-Diasca archive, probably corresponding to a stable version (ex: ``Sim-Diasca-a.b.c.tar.bz2``) or to a source snapshot named after its date, in the form ``Sim-Diasca-a.b.c-beta-yyyymmdd.tar.bz2`` (ex: ``Sim-Diasca-2.0.12-beta-20120417.tar.bz2``). Note that since the 2.0.0 version we use `Semantic Versioning <http://semver.org/>`_.

In a directory on which you have read/write access and enough space left, extract that Sim-Diasca archive, using a proper ``tar`` incantation:

:raw-html:`<img src="xkcd-tar.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-tar.png}`


For example::

  $ tar xvjf Sim-Diasca-a.b.c.tar.bz2

This should create a root directory named ``Sim-Diasca-a.b.c`` which contains all the relevant sources, including various top-level directories (``common``, ``wooper``, ``traces``, ``sim-diasca``, etc.).

From now on, non-absolute paths (ex: ``sim-diasca/tools/...``) must be understood as being relative to this root directory.



Installation From The Sim-Diasca GIT Repository
...............................................

If you have access to the Sim-Diasca GIT repository (internal to EDF R&D), then you can perform a clone of all Sim-Diasca sources instead of relying on an archive.

You just have to issue (if you target the current trunk rather than any specific branch or tag)::

  $ git clone git+ssh://<user>@cln46mr.der.edf.fr/home/git/Sim-Diasca


For example::

  $ git clone git+ssh://boudevil@cln46mr.der.edf.fr/home/git/Sim-Diasca


Installing Erlang
=================

Sim-Diasca is mostly written in Erlang, thus as soon as it will have to run on a given host, it will require a proper Erlang environment to be available on this host beforehand.

This means that all the computing hosts that may be used in the context of a distributed simulation must have access to such an Erlang environment, with compatible versions. There are various ways of ensuring it, including the cases where:

 - an appropriate Erlang environment is built-in on the host operating system
 - the hosts have access to some shared infrastructure (ex: a distributed filesystem, like NFS)
 - a dedicated installation is performed on each of them

Although older versions of Erlang are supported, starting from ``R12B-5`` (released on November 5, 2008), we strongly recommend to rely on the latest stable version available, as it is both more robust and efficient. This version is usually in the form ``RxBy``, like ``R16B``, at the time of this writing.

Erlang will then be preferably built from sources rather than installed thanks to the distribution, in order to benefit from an unmodified cutting-edge stable version which additionally will be built with the finely-tuned configuration deemed the most appropriate in the Sim-Diasca context.

.. Note::

  The libncurses development files (ex: the ``libncurses5-dev`` package, for Debian-based distributions) are needed to build Erlang.

  Similarly, the OpenSSL development files (i.e. the ``openssl`` and ``libssl-dev`` packages, for Debian-based distributions) are needed for the ``crypto`` Erlang module to be available, as we prefer to rely on better-quality random generation - we may use Tiny Mersenne Twister (TinyMT) in the future.

  Finally, newer Erlang graphical services rely on WxWidgets, whose packages (notably ``libwxgtk2.8-dev`` and their own prerequisites, ``libgl1-mesa-dev``, ``libglu1-mesa-dev`` and ``libpng3`` and ``g++``) are preferably to be installed (needed for the ``wx`` and ``observer`` modules for example).

  Therefore all these packages should be installed beforehand. Build tools, notably gcc and GNU make, should be available as well.

  So, to obtain a proper Erlang installation, users of Debian-based distributions may run the following command:
  ``sudo apt-get install g++ make libncurses5-dev openssl libssl-dev libwxgtk2.8-dev libgl1-mesa-dev libglu1-mesa-dev libpng3``.



In the ``common/conf`` directory of the extracted Sim-Diasca archive, there is a script named ``install-erlang.sh``.

If you have a direct connection to the Internet, it can automatically download the Erlang sources, and then build and install them appropriately.

You can either run that script "as is" (with or without a prefix being specified as parameter) or, if preferred, modify its settings appropriately beforehand, or just get inspiration from it instead and install Erlang directly from the shell.

``install-erlang.sh --help`` will provide more usage information.

One can run for example::

	$ ./install-erlang.sh

or, if a specific prefix is to be used::

	$ ./install-erlang.sh /opt/my-tools-repository


In all cases, you should end up with an installed version of the latest stable source of Erlang.

Sim-Diasca developers could prefer installing automatically the latest cutting-edge version instead (if would be a beta one, if available) and also the associated documentation. This could be done thanks to::

	$ ./install-erlang.sh --cutting-edge --doc-install


A beta version is more recent than the latest stable version. It is not production-ready, yet usually already very stable. Their name is in the form of ``RxA``, like ``R14A``.

Let's call ``V`` the version number selected by the script (ex: ``V=R16B``).

The actual installation directory will then be:

 - if no prefix was specified:

   - if the install script is run as root, Erlang will be directly installed in ``/usr/local``
   - otherwise: in ``~/Software/Erlang/Erlang-$V``

 - if a prefix PREFIX was specified, installation will be done in ``PREFIX/Erlang/Erlang-$V``


If intending to make any actual development in the future (ex: writing a specialized simulator, adding models or operating on the Sim-Diasca code itself), one should add the ``--generate-plt`` option to the ``install-erlang.sh`` command-line. It will pre-process Erlang files to generate a PLT file that will be later reused by the `Dialyzer <http://www.it.uu.se/research/group/hipe/dialyzer>`_ tool for code analysis. Please refer to the ``Using Type Specifications With Sim-Diasca`` section of our user guide for further information.

Running the installation script should create, in the target installation directory, two corresponding sub-directories, ``Erlang-$V`` and ``Erlang-$V-documentation``, containing respectively the Erlang runtime and its corresponding documentation, if it was selected.

Additionally, in this installation directory two symbolic links (``Erlang-current-install`` and ``Erlang-current-documentation``) will also be automatically created or updated, to point to these newly installed directories, so that one can register in one's settings files (ex: ``~/.bashrc``) appropriate paths referring to these **links**: further Erlang updates would then not require the user to update his settings, while prior installed versions will remain available through the use of their full path.

So one may end up with a directory layout like::

  > tree -L 1 -d ~/Software/Erlang/
  /home/boudevil/Software/Erlang/
 |-- Erlang-R14B
 |-- Erlang-R14B-documentation
 |-- Erlang-R16B
 |-- Erlang-R16B-documentation
 |-- Erlang-current-documentation -> Erlang-R16B-documentation
 `-- Erlang-current-install -> Erlang-R16B



In the general case (i.e. unless run as root with no prefix specified), the new Erlang environment will be installed in a prefix, thus probably it will not be readily available from the shell. As a consequence one should ensure that the Erlang compiler (``erlc``) and the corresponding interpreter (``erl``) [#]_ can be found directly from the ``PATH`` (both are in the same directory).

.. [#] The Erlang compiler transforms Erlang code (in ``*.erl``/``*.hrl`` files) into BEAM bytecodes (``*.beam``) suitable for the *Erlang Virtual Machine*. They can then be executed by the Erlang interpreter.

For example, directly from a ``bash`` shell::

	$ export PATH=/home/boudevil/Software/Erlang/Erlang-current-install/bin:$PATH
	$ cd ~
	$ type erl
	erl is /home/boudevil/Software/Erlang/Erlang-current-install/bin/erl


Setting also the relevant path, one time for all (rather than on a short-lived terminal), in the shell configuration of the user (ex: ``~/.bashrc``) is mandatory for further uses as well.

Finally, two simple tests allow to ensure that Erlang can run flawlessly in this new environment. First one allows to check that we are using the expected version and that it can indeed be run (you have to enter CTRL-C twice to close the Erlang shell)::

 $ cd
 $ type erl
 erl is /home/boudevil/Software/Erlang/Erlang-current-install/bin/erl
 $ erl
 Erlang R16B (erts-5.10.1) [source] [64-bit] [smp:8:8]
 [async-threads:0] [hipe] [kernel-poll:false]

 Eshell V5.10.1  (abort with ^G)
 1>


Second test allows to check that your network configuration allows to run a *networked* Erlang virtual machine with long names (enter again CTRL-C twice to exit)::


 $ erl -name this_is_a_test
 Erlang R16B (erts-5.10.1) [source] [64-bit] [smp:8:8]
 [async-threads:0] [hipe] [kernel-poll:false]

 Eshell V5.10.1  (abort with ^G)
 (this_is_a_test@foo.bar.org )1>


Refer to the `Name Resolving`_ section should this test fail.



Installing LogMX
================

LogMX is the default tool used here to monitor the distributed simulation traces. Although its purpose is only to allow to supervise the Sim-Diasca traces, its installation requires quite a lot of explanations, especially to deal with the case where the Sim-Diasca parser for LogMX has to be rebuilt from its sources.


A prerequisite is to have the ``Java SE Runtime Environment`` installed on the host, preferably the (free software) OpenJDK version.

For example::

  $ java -version
  java version "1.7.0_40"
  OpenJDK Runtime Environment (IcedTea 2.4.1) (ArchLinux build 7.u40_2.4.1-3-x86_64)
  OpenJDK 64-Bit Server VM (build 24.0-b50, mixed mode)


Otherwise the ``Sun`` version could be used, like in::

  $ java -version
  java version "1.6.0_10"
  Java(TM) SE Runtime Environment (build 1.6.0_10-b33)
  Java HotSpot(TM) Client VM (build 11.0-b15, mixed mode, sharing)



If not available, either the package manager of the distribution [#]_ or this `link <http://java.sun.com/javase/downloads/index.jsp>`_ for the Sun version should be used.

.. [#] Example: ``apt-get install sun-java7-jdk``, ``apt-get install openjdk-7-jdk`` or ``pacman -S jdk7-openjdk``, for Arch Linux.


Note that if only the ``Java SE Runtime Environment`` (i.e. the JRE) is installed (instead of the ``Java SE Development Kit``, i.e. the JDK), then Java code can be executed indeed, but not generated.

However both cases should work, since using a recent JRE should spare the rebuilding of the Sim-Diasca parser (and hence the use of the JDK).

.. Note:: With some distributions (ex: Ubuntu), the default JRE is headless (which means that the JRE does not provide the dependencies used for the graphical components). Fixing this error is as easy as installing the missing dependencies (``apt-get install openjdk-7-jre``).



Getting LogMX
-------------

LogMX is a proprietary software, thus it cannot be found in package repositories.
It should be directly downloaded from `that location <http://www.logmx.com/p_download.php>`_, in its latest version, either the free ``Evaluation`` version or the ``Professional`` one (both of which can be used by Sim-Diasca).


.. Note::
  If you plan to make a commercial use of LogMX, then according to its licence you must purchase its quite inexpensive professional version. In this case all paths in the form of ``LogMX_vx.y.z`` should be translated into paths in the form of ``LogMX_vx.y.z_pro``.


LogMX archive should be extracted, preferably in the same install directory as other tools.

For example::

	$ mkdir -p ~/Software/LogMX
	$ cd ~/Software/LogMX
	$ cp ~/LogMX_vx.y.z.zip .
	$ unzip LogMX_vx.y.z.zip

It must then be appropriately configured and linked to a proper Sim-Diasca parser, as shown below.



Setting Up LogMX
----------------


Configuration Files
...................

Sim-Diasca provides, in the ``traces/conf/logmx`` directory, the following configuration files:

 - ``logmx.properties``
 - ``managers.properties``
 - ``parsers.properties``

They should be copied in the LogMX ``config`` directory. These files should overwrite the default LogMX ones. For example::

	$ for f in logmx.properties managers.properties parsers.properties; do \
	/bin/cp -f traces/conf/logmx/$f ~/Software/LogMX/LogMX_vx.y.z/config ; \
	done


.. Note:: If you purchased the LogMX professional version, copy the ``license.properties`` file that you obtained in the LogMX ``config`` directory, instead of using the SimDiasca-provided one.


The LogMX script must then be set to executable::

	$ chmod +x ~/Software/LogMX/LogMX_vx.y.z/logmx.sh

Identically to ``Erlang``, the LogMX script must be found from the path. For example, with a ``bash`` shell::

	$ export PATH=$HOME/Software/LogMX/LogMX_vx.y.z:$PATH
	$ cd ~
	$ type logmx.sh
	logmx.sh is /home/boudevil/Software/LogMX/LogMX_vx.y.z/logmx.sh

Setting also the relevant path in the shell configuration (ex: ``~/.bashrc``) is recommended for further uses.

A best practise for that is to install all custom software in a base directory (ex: ``$HOME/Software/``), with a sub-directory for each tool (ex: ``$HOME/Software/LogMX/``). Then all successive versions of that tool could be installed here (ex: ``$HOME/Software/LogMX/LogMX_v4.0.2/``).

Finally, a symbolic link pointing to the latest current version could be defined when installing a new version of that tool (ex: ``cd $HOME/Software/LogMX/; ln -sf LogMX_v4.0.2 LogMX-current-install``).

That way, one just has to specify in one's shell configuration::

  export PATH=$HOME/Software/LogMX/LogMX-current-install:$PATH


This is thus done once for all, it will not have to be updated when upgrading LogMX.

LogMX should then be run "as is", to ensure that it has a chance to run later, when the Sim-Diasca parser will be plugged-in::

	$ logmx.sh

After up to a few seconds, a LogMX window should successfully pop up. Then close that window.

.. Note::
   On some recent LogMX versions, running this ``logmx.sh`` script will output a line on the console complaining about a ``startup.conf`` file being not found, or printing ``[: 86: 1: unexpected operator`` and ``[: 86: 0: unexpected operator``.

   A simple solution is to edit ``logmx.sh`` and replace the ``STARTUP_CONF_FILE="startup.conf"`` line (around line 35) by ``STARTUP_CONF_FILE=/dev/null``.





Setting Up the Sim-Diasca Trace Parser
......................................

Due to Java, this is probably the trickiest part of a Sim-Diasca install.


Using The Prebuilt Sim-Diasca Parser
____________________________________

In the ``traces/conf/logmx`` directory, there is a prebuilt Java class, ``CeylanTraceParser.class``, a generic parser we developed for Sim-Diasca and other tools.

If the Java environment installed on the host is recent enough, then that class file will be directly usable, without further need of recompiling it.

Best option is to try to use it directly, and to rebuild it only if this fails.

That file should just be copied to the right location::

 $ CLASS_DIR=~/Software/LogMX/LogMX_vx.y.z/parsers/classes/ceylan/parser
 $ mkdir -p $CLASS_DIR
 $ cp traces/conf/logmx/CeylanTraceParser.class $CLASS_DIR



Checking That The Sim-Diasca Parser Works Properly
__________________________________________________

To do so, just test, from the root of the sources, whether LogMX and the Sim-Diasca parser are correctly integrated, with a sample of Sim-Diasca traces::

	$ logmx.sh traces/conf/logmx/TraceSample.txt


You can skip next section if you see something like:

:raw-html:`<img src="logmx-interface.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{logmx-interface.png}`

Otherwise, an error like ``Error while instantiating parser`` must have been reported: your Java environment is most probably not appropriate (too old?), and, **if** you are not able to upgrade the Java interpreter that you are using, then unfortunately the parser will have to be rebuilt with the Java bells and whistles, as explained in the next section (usually this issue does not occur, and one can thus jump directly to the `Checking Which Tools Sim-Diasca Will Use`_ section).


Building The Sim-Diasca Trace Parser
____________________________________


The ``Java SE Development Kit`` (i.e. the JDK) and ``Ant`` are needed here.

They can be installed either thanks to the distribution, for example::

  $ sudo apt-get install openjdk-6-jdk ant
	- or -
  $ sudo apt-get install sun-java6-jdk ant

or they can be retrieved from their respective official sites (`1 <http://java.sun.com/javase/downloads/index.jsp>`_, `2 <http://ant.apache.org>`_), if not directly built and installed from sources (for Ant).


Then the Sim-Diasca parser source file should be placed at the right location in the LogMX tree, and built::

 $ PARSER_SRC_DIR=~/Software/LogMX/LogMX_vx.y.z/parsers/src/ceylan/parser
 $ mkdir -p $PARSER_SRC_DIR
 $ cp traces/conf/logmx/CeylanTraceParser.java $PARSER_SRC_DIR
 $ cd ~/Software/LogMX/LogMX_vx.y.z/parsers
 $ ant
 Buildfile: build.xml
 clean:
 mkoutdir:
  [mkdir] Created dir: ~/Software/LogMX/LogMX_vx.y.z/parsers/classes
 build-dev:
  [javac] Compiling 1 source file to ~/Software/LogMX/LogMX_vx.y.z/parsers/classes
 BUILD SUCCESSFUL
 Total time: 2 seconds

This should imply that ``CeylanTraceParser.class`` has been successfully built.

Test the result like explained before, in `Checking That The Sim-Diasca Parser Works Properly`_.





Checking Which Tools Sim-Diasca Will Use
========================================

It is mandatory to have Sim-Diasca know where the tools it needs can be found. To check which main tools would be used, run from the ``sim-diasca`` directory::

	$ make info-tools
	ERLANG_INTERPRETER = ~/Software/Erlang/Erlang-current-install/bin/erl
	ERLANG_COMPILER = ~/Software/Erlang/Erlang-current-install/bin/erlc
	LOGMX = ~/Software/LogMX/LogMX-current-install/logmx.sh



Some tools will be only used by this Make system, whereas others, the majority of them (ex: the Erlang interpreter and compiler) will be used by the simulator as well.

Therefore the path to the former ones could be set directly in the makefiles only. However it is generally more convenient that the latter ones are found directly from the shell environment, so that both the Make system *and* the simulator will find them with the same correct versions.

If a Make-only tool is lacking, edit the ``GNUmakevars.inc`` file of the relevant package (ex: the one of ``common``, ``wooper``, ``traces``, ``sim-diasca``, etc.) accordingly.


If another tool is lacking, then the shell environment should be updated. This involves updating - most preferably, once for all - the PATH environment variable.

This can be done by adding ``PATH=/a/path/to/a/lacking/tool:$PATH`` to the shell init file (ex: ``~/.bashrc``) and sourcing it again (``. ~/.bashrc``).

Re-run ``make info-tools`` and apply changes until the Make system selects the exact tool versions you want.



:raw-latex:`\pagebreak`


Building Sim-Diasca
===================

The good news is that Sim-Diasca is written in Erlang, thus it requires to be compiled:

:raw-html:`<img src="xkcd-compiling.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-compiling.png}`


The bad news is that it will not take long, only up to a few minutes, as it is itself parallel on each package.

It is just a matter of running ``make`` from the Sim-Diasca source root::

   $ cd Sim-Diasca-a.b.c
   $ make all
   Building all, sequentially, in [..]/Sim-Diasca-a.b.c
   Building all, in parallel over 8 cores, from [..]/common
   Building all in [..]/Sim-Diasca-a.b.c/common/contrib
   Building all in [..]/Sim-Diasca-a.b.c/common/src
				Compiling module hashtable.erl
				Compiling module hashtable_test.erl
				Compiling module hashtables_comparison_test.erl
		[...]
	Building all, in parallel over 8 cores, from [..]/wooper
	[..]
	Building all, in parallel over 8 cores, from [..]/traces
	[..]
	Building all, in parallel over 8 cores, from [..]/sim-diasca
	[..]
	Building all, in parallel over 8 cores, from [..]/mock-simulators
	[..]



:raw-latex:`\pagebreak`


Enabling The Distributed Mode Of Operation
==========================================

A key point of scalability lies in the possibility of harnessing distributed resources.

.. Note:: Should Sim-Diasca be run only locally (i.e. on the current computer), this section can be safely skipped.

In a distributed context, Sim-Diasca must be able to make use of the computing resources available in other networked nodes.

To do so, the Sim-Diasca agents must be already running - and thus be already installed - on each of the targeted nodes before a simulation relying on them is run.

Of course, the user could log on each of these nodes, and install then launch manually the agents needed, however this process would be quite cumbersome and could not scale up. Sim-Diasca can fully perform this deployment task on the user's behalf instead (installation and execution), if proper settings are used.

One has mainly to ensure that the network is correctly configured and that, with one's account, a SSH password-less login can be performed from the current computer to all targeted remote nodes, which are expected to already have an Erlang environment directly available.

From now on, the *user node* will designate the Erlang node from which the user will run the simulation (ex: the one created when issuing a command like ``make my_simulation_run`` from the user shell). That node will never take part directly to the computing. However, depending on the simulation settings, the host this node runs on may or may not be used as a computing resource, thanks to the automatic creation of another (local, simulation-dedicated) computing node.




Basic Network Configuration
---------------------------

.. Note:: Sim-Diasca will check automatically all the points discussed in this section, whose purpose is only to inform the reader and to provide some guidelines, should a check determine that the network is not properly configured.



Interconnectivity
.................

All hosts able to take part to a (distributed) simulation should be able to reach all other hosts thanks to the network.

To check that at least a basic interconnectivity is available, Sim-Diasca will ping all eligible hosts from the user host, and retain only the alive ones (i.e. hosts that cannot be ping'ed are deemed not available for the simulation) [#]_.

.. [#] Note however that we could imagine that the user host pings successfully ``h1`` and  ``h2``, while ``h1`` is still not able to communicate with ``h2``. However such awkward network configuration issues seldom occur.

Once this host interconnectivity is established, node interconnectivity will be automatically checked further.



Name Resolving
..............

Each computing host should, in terms of network, be correctly configured.

Notably, for an host whose fully qualified DNS name is expected to be ``hurricane.example.org``, one should have indeed::

 $ hostname
 hurricane

 $ hostname -f
 hurricane.example.org


In the latter case, returned values such as ``localhost.localdomain`` or ``localhost`` mean that the host will not be able to take part to a networked simulation.

Usually this configuration can be fixed if having root permissions. The ``/etc/hosts`` file should indeed have a line similar to::

  127.0.0.1 hurricane.example.org hurricane localhost.localdomain localhost

Note the order: the fully qualified DNS name (``hurricane.example.org``) should be the first entry listed after the ``127.0.0.1`` IP, otherwise the networked mode of operation of Erlang may not be activated.



Security
........

All nodes created by Sim-Diasca will be given the unique cookie forged (based in a UUID) by the user node. So no two simulations can collide, even if the same case is run by the same user on the same computers.



Password-less Authentication
----------------------------

This can be done with SSH quite simply, without requiring specific permissions [#]_, but thanks to a proper configuration.

Let's suppose a user ``foobar`` on host ``host_a.example.org`` wants to set-up password-less connections to ``host_b.example.org`` (supposedly with the same username).


.. [#] Provided that the SSH server running on the target host allows it (see the ``PubkeyAuthentication yes`` entry in the ``/etc/ssh/sshd_config`` file); it is generally the case.


``foobar`` just has to generate, when logged on ``host_a.example.org``, a key pair with::

  foobar@host_a.example.org$ ssh-keygen -t rsa
  Generating public/private rsa key pair.
  Enter file in which to save the key (/home/foobar/.ssh/id_rsa):
  Enter passphrase (empty for no passphrase):
  Enter same passphrase again:
  Your identification has been saved in /home/foobar/.ssh/id_rsa.
  Your public key has been saved in /home/foobar/.ssh/id_rsa.pub.
  The key fingerprint is: XX:XX:XX... foobar@host_a.example.org

It generates a private key (``~/.ssh/id_rsa``, which shall be kept as is) and a public one (``~/.ssh/id_rsa.pub``, which can be freely shared).

The user then just has to register that public key to ``host_b.example.org``.

On GNU/Linux this can be done simply thanks to::

  foobar@host_a.example.org$ ssh-copy-id -i \
	~/.ssh/id_rsa.pub host_b.example.org


If the above command fails or is not available, then the following procedure should be used.

First the public key must be transferred to the remote host::

  foobar@host_a.example.org$ scp ~/.ssh/id_rsa.pub \
	foobar@host_b.example.org:/home/foobar/.ssh/id_rsa-from-host_a.pub


Then it just has to be declared as authorised on that remote host, with the proper permissions::

  foobar@host_b.example.org$ cat /home/foobar/.ssh/id_rsa-from-host_a.pub \
	>> /home/foobar/.ssh/authorized_keys
  foobar@host_b.example.org$ chmod 600 /home/foobar/.ssh/authorized_keys
  foobar@host_b.example.org$ chmod 700 /home/foobar/.ssh


Once either of the two approaches succeeded, with a basic proper configuration of the SSH server, the login to the remote host should be performed without needing a password::

  foobar@host_a.example.org$ ssh host_b.example.org
  last login: XXX
  foobar@host_b.example.org$


.. Hint::
  Each time a new computing host is added, one should preferably attempt to perform from the command line a connection from the user node to this new node, in order to ensure that no interactive acknowledgement is needed.

  For example, following message could be prompted once (some measures have been taken to avoid it, though):

  ``The authenticity of host 'Server (XXXXX)' can't be established. RSA key
  fingerprint is YYYYY. Are you sure you want to continue connecting (yes/no)?``.

  Just answer yes (once for all).


Note finally that when using multiple computing nodes, all nodes should be able to connect to all nodes. More precisely, the first constraint is actually that the host on which the deployment manager will be run (i.e. the user node) should be able to connect by SSH to all other hosts (to spawn the remote computing nodes).

Then, once the deployment phase is over, the load balancer may spawn a model instance on any of the computing nodes, and two model instances may interact (between them and with the various simulation services) regardless of their respective actual locations. Therefore a fully-meshed connectivity (everyone to everyone) is needed.

Note also that, notably for the sake of homogeneity, if the host from which the user launched the simulation has been listed into the eligible computing hosts, then another node will be created on the same host (an additional computing node), rather than reusing that launching (user) node as a computing one. No SSH connection between these two local nodes will be attempted.


Finally, for an host named ``foo.bar.org``, a simple test allows to check both the SSH configuration and the availability of an Erlang environment. One should just run for each of the computing host::

  $ ssh -q foo.bar.org erl -eval \
	   '"io:format( \"This host would use Erlang version ~s.~n\", \
	   [erlang:system_info(otp_release)]), erlang:halt()."'

  Eshell V5.10.1  (abort with ^G)
  This host would use Erlang version R16B.


In that case, this host should be able to take part to the simulation, provided the firewall policy does not prevent it [#]_.

.. [#] Note that Sim-Diasca provides a way of specifying a non-standard EPMD port and a range of allowed TCP ports (see ``FIREWALL_OPT`` in ``common/GNUmakevars.inc`` or, preferably, the ``firewall_restrictions`` field of the ``deployment_settings`` record). However the usual practice, and by far safest, is to avoid the use of specific firewall policies between internal hosts: there are already plenty of reasons for a distributed application to experience runtime issues, no need to add more potential problems.


Note also that the Sim-Diasca host configuration file allows to specify which UNIX user should be used on which host (as one may use different logins on different hosts).



Managing the Simulator Codebase
-------------------------------

In a distributed context, the proper code (software) must of course be available on each of the computing hosts, so that this host is able to run a part of the simulation.

However installing manually everything on each host is soon strenuous, error-prone, and could happen many times (ex: when fine-tuning some models).

With Sim-Diasca, the only prerequisites that must be available on a host before this host is able to take part to a simulation are:

 - having a proper configuration of the computing host, as already described (regarding network, SSH and al)

 - and having a proper Erlang environment available on that host (i.e. ``erl`` must be directly found on the shell after a non-interactive login), preferably installed once for all with the ``common/conf/install-erlang.sh`` script we provide, or installed globally on a NFS mount point


Then Sim-Diasca is able to launch automatically the proper agents on each of the selected computing hosts, and to transfer and set up on that host not only the simulation models to be used, but also its own code and prerequisites as well (i.e. the ``common``, ``wooper``, ``traces`` and ``sim-diasca`` packages).

So Sim-Diasca provides an automatic deployment system for the full simulator.

Such a deployment is based on a deployment archive (a ``.sar`` file, for *simulation archive* - a compressed, binary file automatically sent over the network), which can be either prebuilt once, and then just specified to each Sim-Diasca run, or that can be automatically built on the fly by Sim-Diasca, from the build tree it is run from on the user node (this is the default case).



Miscellaneous
-------------

One should ensure that the ``/tmp`` directory is writable by the user on each computing node, and that sufficient free space is available.


Cleaning Up
-----------

Under some circumstances (ex: a failure encountered during a simulation), some pending nodes, created by the deployment manager, may linger on a computing host.

To remove them, the *Erlang Port Mapper Daemon* can be used, just run of that host::

  $ epmd -kill


Note that launched nodes are, except on some error cases, automatically cleaned up and shut down by Sim-Diasca on simulation termination.

Should a simulation crash prevent this removal, all pending nodes will stop by themselves after a time-out (set by default to 10 wall-clock minutes, enforced by all local time managers).

Additionally, each time a simulation is run, by default it will start by attempting to remove any still lingering node it would detect.

Finally, should a newly run simulation discover that such pending nodes are still alive despite the counter-measures taken, it will detect it (based on the run-specific UUID that is generated first), and will exclude the corresponding hosts for that run.


.. comment Any pending SSH connection can be removed thanks to::

  $ killall ssh
  Note that *all* currently running SSH connections are in this case expected to be removed.




:raw-latex:`\pagebreak`


Testing Sim-Diasca
==================

Several test cases that can be run to experiment with Sim-Diasca: when a class ``X`` is defined (in ``class_X.erl``), it is recommended to add a corresponding unitary test case (in ``class_X_test.erl``).

To run such a test, once Sim-Diasca has been successfully built, one just has to go to the directory where that test is defined, and to run ``make class_X_run``: the Sim-Diasca Make system will take care of compiling this test if needed and run it with an appropriately-configured Erlang interpreter.

For example, if wanting to run a Sim-Diasca built-in soda-vending test::

	$ cd sim-diasca/src/models/examples/src
	$ make
	$ make soda_deterministic_integration_run


Three windows should pop up [#]_:

	- a first ``Geeqie/Gqview`` window, displaying the two simulation results (two time series) as graphs (plots), representing the number over time of cans available in each of the two soda vending machines
	- a second ``Geeqie/Gqview`` window, displaying the measures aggregated by the performance tracker (resource consumption, number of instance per node, etc.), if this service is enabled (true by default)
	- a ``LogMX`` console, for the supervision of the distributed simulation traces (if traces are enabled, which is true by default)


.. [#] When running a test from the command line, simulation results are displayed, whereas when the same test is run as part of a test suite, its outputs are generated but not displayed, since it must be run in batch (i.e. silently). Note that how tests are run regarding their outputs is unrelated to the operation mode of the time manager (batch or interactive mode).

When not useful any more, all windows can be safely closed. The end of the simulation session occurs when the trace supervision window is closed.

There are also a few integration tests (involving interacting models). For example::

	$ cd sim-diasca/src/models/equipment/src
	$ make equipment_integration_run
	 Running unitary test equipment_integration_run (third form)
	 from equipment_integration_test
	[..]
	Watchdog removed (deletion).
	[Trace Supervisor] Trace supervisor ended monitoring of
	'equipment_integration_test.log' with LogMX.
	[Trace Supervisor] Trace supervisor created.
	(test finished, interpreter halted)


A ``Geeqie/Gqview`` window, displaying the three plots output by the reliability probes of the three test equipments, and a ``LogMX`` console should pop up.

Finally, all tests can be run in batch mode from any level of the Sim-Diasca tree, including from the top directory::

	$ make test
	Building all in [..]
	[..]
	Testing all in src
	[..]
	 Running unitary test basic_utils_run [..]
	[..]


If using a Sim-Diasca (stable) release version, all tests should be run successfully, whereas some tests might be broken if using a Sim-Diasca development version.

To further discover how Sim-Diasca works and can be used, the next steps could be to peer in the source code of tests and of classes, before playing around and adding some toy models.



Installing Sim-Diasca
=====================

This completely optional action allows to install all Sim-Diasca related packages (i.e. the ``Common``, ``WOOPER``, etc. packages) out of the build tree.

To do so, one just has to execute, from the top source directory (the one that contains the top-level directories like ``sim-diasca``, ``wooper``, etc.)::

  $ make install

In this case everything will be installed in the default ``~/Software`` directory, which will be created if not existing already.

The user can specify any other installation directory instead, by defining the ``INSTALLATION_PREFIX`` variable, like in::

  $ make install INSTALLATION_PREFIX=/opt/my-simulator


In all cases, under the installation directory, all Sim-Diasca related packages will be properly installed, mostly according to the Erlang recommended practises (i.e. with a hierarchy based on standard nested directories like ``ebin``, ``examples``, ``include``, ``src``, ``test``, etc.).
