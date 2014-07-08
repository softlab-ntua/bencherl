:raw-latex:`\pagebreak`


------------------
Sim-Diasca Changes
------------------


.. Note:: All the changes can be tracked down, with full details, based on the R&D internal GIT repository (see the ``git log`` command).


Starting from the 2.x versions, we did extensive efforts to preserve backward compatibility, so that most of the pre-existing user workflows are preserved.

:raw-html:`<img src="xkcd-workflow.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-workflow.png}`




Pre-Releases: 0.x Versions
==========================

Starting in 2008, and up to mid-2009, various initial versions of Sim-Diasca (initially named Sim-MS) were pre-released, including three more official ones (0.1, 0.2 and 0.3).



Releases of the 1.x Versions
============================

As of Tuesday, July 21, 2009, the Sim-Diasca 1.0 version was pre-released.

Quite similar to the previous versions, its main purpose was to be a milestone so that the work on the 2.x versions could begin.

Among the improvements, we had:

 - a better source layout, with the dispatching of files in ``core/src`` into thematical directories and a clean separation of the trace system into a package of its own

 - all tests (including the ones in ``models``) now compile and run correctly (newer stochastic management was integrated)

 - traces can be aggregated as text files or PDF reports, instead of LogMX traces

 - an installation mechanism is now available, but its used is optional



Releases of the 2.x Versions
============================

Work started at the end of July, 2009, and the target pre-release was hit in the course of October, 2009.


Versions 2.0.0 to 2.0.4
-----------------------

 =======    ============================
 Version    Release Date
 =======    ============================
 2.0.0      Friday, October 26, 2009
 2.0.1      Friday, November 13, 2009
 2.0.2      Wednesday, November 18, 2009
 2.0.3      Wednesday, November 25, 2009
 2.0.4      Tuesday, December 22, 2009
 =======    ============================

Major changes included:

 - automatic distributed deployment and execution of the simulator
 - enhanced distributed mode of operation
 - support for more complex classes of actor scheduling
 - WOOPER update

Less significant changes includes:

 - backporting of the Ceylan generic code which was forked for Sim-Diasca, to avoid the duplication of efforts
 - second (and, hopefully, last) renaming of *Sim-MS* (initially *Sim-IS*) to *Sim-Diasca*
 - better tick management, based on smaller offsets rather than large absolute time references




Version 2.0.5
-------------

Released on Tuesday, February 9, 2010; main changes were:

 - in 'common':

   - integrated ``text_utils``

 - in 'traces':

   - explained why some simulations were lingering because of the trace aggregator (reason: too many traces sent, actors to verbose, aggregator overwhelmed)

 - in WOOPER:

   - when a timed synchronous creation of an instance fails, an exception is directly raised (ex: ``{synchronous_time_out,?MODULE}`` is thrown), instead of just returning the ``time_out`` atom), to avoid a late detection of the problem

 - in Sim-Diasca:

   - fixed a very infrequent scheduling error which could happen due to a race condition between a ``schedule_trigger`` message and the next ``top`` message (bug caught thanks to the newer actor-side checking mechanisms)
   - better in-depth checking of scheduling correctness (most complete checks performed)
   - last diagrams were updated
   - documentation updated a bit (ex: newer hints)
   - the load balancer is now an actor as well, thus actor creations at simulation-time are themselves reproducible, and thus context-free full reproducibility should be guaranteed



Version 2.0.6
-------------

Released on Thursday, April 8, 2010.

Main changes were:

 - in ``Common``, among a large number of other helper functions, now sorted in topic-based subdirectories, smart automatic waiting for file descriptors has been added ; selection of compilation options improved (ex: warning are now treated as errors, at last)

 - in ``WOOPER``, error messages were still more clarified and better pretty-printed, and all the state management functions (like setAttribute/3) are now fully inlined by the compiler, thanks to proper and specific compilation directives (thus no performance penalty at all compared to the legacy macros)

 - in ``Traces``:

   - runtime performance of the trace system increased; notably: use of binaries instead of plain strings, expecting a gain of a factor x7 in terms of size; finally, after another improvement at the level of the write policy for traces, measured an overall decrease of x68.6 in terms of runtime duration for the same test case (``traceManagementUnderPressure_test.erl``); nevertheless model developers should ensure that actor-level traces are kept under a reasonable threshold, lest the trace aggregator becomes overwhelmed and explodes in memory (if traces are sent faster than it can process them, as there is no actual control flow, since trace sending is asynchronous, for obvious performance reasons)

   - on the user side, all trace macros have been vastly improved (simplified, more varied, never triggering any warning when deactivated), and their newer macros have already been integrated into Sim-Diasca

   - newer major version of LogMX integrated and used (2.0.0); our Java Trace Parser has been ported to the new API and takes advantage of the new features, including faster interface rendering and management of user-defined fields in supervised traces (now these information are stored in Sim-Diasca specific fields, which can be sorted and searched as such, instead of having to be aggregated in the message field)

 - in ``Sim-Diasca``:

   - improved diagnoses are determined in case of simulation stall

   - probes can now write their command file either initially or only when generating a report (new default), and can either store data samples or write them on-the-fly (new default, for a decreased memory footprint); moreover now new curves may be declared dynamically, whereas former ones have already gathered data; when the report will be generated all curves will be rendered appropriately; additionally, partial samples can now be sent, for curves that do not have relevant data for a sampled tick; see updated documentation for more information

   - data-logger added, relying on the embedded full-blown distributed soft realtime database (Mnesia); the database is automatically deployed and managed, and supports virtual probes, which are less limited (should be more scalable process-wise), more powerful versions (features added like transactional sample merging) of the standard Sim-Diasca probes; non-consecutive ticks supported, even unordered ones, setting and merging can be synchronous (for flow control) or not (for best performance), can be managed by the data-logger or can be directly done by the sample source so that the operation can remain fully local to the same node (for more information see the ``datalogging_test.erl`` test, in ``sim-diasca/src/core/src/data-management/data-storage``)

   - run-time information about the simulation state are now displayed in the console (S-T-R-A-P information); a flow control system allows to guarantee that up to one console output per second will be performed (useful when computing resources allow to compute many ticks per wall-clock second, otherwise the simulation could be slowed down because of console output)

   - pre and post-simulation node cleaning improved, moreover any orphaned node will stop automatically after 10 minutes of inactivity

   - performance tracker added, to monitor over (wall-clock) time notably the detailed life-cycle of processes, WOOPER instances and Sim-Diasca actors



Version 2.0.7
-------------

Released on Thursday, June 10, 2010.

Main changes were:

 - in ``Common``:

   - opening of files when lacking file descriptors leads now by default directly to a clear error rather than a less clear and often unsuccessful attempt of overcoming it

   - using now parallel builds over all available cores, which reduces significantly the build duration (for the developer) and the creation of the deployment package (for the user)

 - in ``WOOPER``:

 - in ``Traces``:

   - now, whenever too many traces are sent (i.e. whenever the trace aggregator is not able to cope with their rate of arrival), a warning message is displayed on the console every 2 seconds, specifying the number of pending traces; formerly, the aggregator was silently growing, and possibly exploding in memory

   - a ``test_receive/0`` function, defined in ``traces_for_tests.hrl``, has been added in order to perform a selective receive which ensures that the message sent by the trace supervision for its internal mode of operation (``monitor_ok``) cannot interfere


 - in ``Sim-Diasca``:

   -  the ``getActorIdentifier/2`` method, ``get_actor_identifier/1`` and ``is_running/1`` helper functions were added, to simplify the writing of models

   - from now on, all simulations will by default always start on Saturday, January 1st, 2000 at midnight (no change of start date from one release to another); use the ``setInitialTick( State, InitialTick )`` or ``setInitialSimulationDate( State, Date={Year,Month,Day}, Time={Hour,Minute,Second} )`` methods of the ``TimeManager`` class, before the simulation is started, to change it

   - more information given at simulation stop (number of ticks elapsed, corresponding virtual duration)

   - lots of improvements regarding the on-console monitoring of simulations (tick tracker, formatted progress array, final synthesis about durations and acceleration factors, version number of the Sim-Diasca version in use)

   - various improvements on the performance tracker (notably: no more error due to timing drift that overloaded computers used to cause)

   - API for simulation listeners improved and enriched

   - placement hints supported now

   - all tests in ``sim-diasca/src/core/src/scheduling/tests`` updated with regard to the newer actor API, some tests added

   - the test actor class has been modified in order that the memory footprint of each instance is increased (of

   - node naming mode (short/long) now fully supported: short names can be used instead of long names, and became the default, as they can resist name resolution issues (non-existing or badly-configured DNS systems); add the ``NODE_NAMING="--ln"`` command-line option to override the defaults for a user node

   - cluster integration done, full support for PBS-based clusters (tested with Torque/Maui); see the launcher scripts in ``sim-diasca/conf/clusters``



Version 2.0.8
-------------

Released on Monday, September 13, 2010.

Main changes were:

 - in ``Common``: various facilities to scan a directory tree added

 - in ``Sim-Diasca``:

   - inter-node firewall policy supported now, see in ``class_DeploymentManager.hrl``, in the ``deployment_settings`` record, the ``firewall_restrictions`` field: a specific TCP/IP port range can be used now, and having EPMD run on a non-standard port can be supported as well

   - probes now by default do not add symbols on top of measured points anymore; this used to lead to visual artifacts, as for examples crosses were accumulating (because there were many points, abscissa-wise), resulting into ugly colored tubes

   - the automatic deployment of third-party code and data is now fully supported; now we can select any number of (data or code) third-party files or directories, with rules to exclude suffixes and sub-directories and/or to include suffixes, and to possibly rebuild targeted data or code

   - thanks to the previous element, a mock SimDiasca-based simulator has been added (see the ``mock-simulator`` top-level directory), as a complete example, to help the development of further SimDiasca-based simulators; two tests are already supported, and they will improve and grow over time:

	 - *Soda-vending machine* test: simple test to show actor creation and interaction (ported from the training material)

	 -  [not ready yet] *SSI* test, for *Sim-Diasca Scalable Integration Test*: first version of the business-free test simulation case, a tiny ecosystem made to load the engine for scalability assessment purpose

 - no more pending nodes rejected when running again a simulation that just crashed on the same computing hosts

 - for the ``future_action`` attribute, in addition to ``passive``, ``terminating``, and a tick offset, a licit value is also ``next_tick`` which schedules a spontaneous behaviour for that actor at its next tick (this saves a ``?getAttr(current_tick_offset)+1`` or a ``class_Actor:get_current_tick_offset()``; particularly useful for first minimal ports for the centralised branch



Version 2.0.9
-------------

Released on Friday, October 29, 2010.

Mainly a BFO (*Bug Fixes Only*) release.

Most significant changes were:

 - in ``Common``: bug in the command-line argument specified for the debug key, found and fixed by Cédric Pasteur (cedric.pasteur at ens.fr)

 - in ``WOOPER``: unexpected messages are now notified with the PID of the receiving instance

 - in ``Sim-Diasca``:

   - default size of the probe reports (canvas) increased significantly (from 640x480 to 1024x768); this size can be set at will now, for each probe; available both for the classical probes and the virtual ones (which had some bugs fixed), see ``class_Probe:setCanvasSize/3`` and ``class_DataLogger:setCanvasSize/4``

   - the newer defaults in terms of ``deployment_settings`` are, for ``computing_hosts``: ``{use_host_file_otherwise_local,"sim-diasca-host-candidates.txt"}`` instead of ``localhost_only`` (most cases will be fine with these defaults)

   - SSI-test updated and fixed

   - documentation being updated, it will be much more pleasing to read thanks to comic strips, yet with roughly the same informative content

   - all variations of placement hints added and tested

   - probe reports can now rely on rotated abscissa labels (xtic), so that the texts corresponding to large tick values are parallel and do not overlap, however long they are (see ``class_Probe:setRotatedTickLabels/1``); large ticks are now displayed as integers, instead of being rounded with an unwanted scientific notation; a probe-specific tick offset can also be defined, to avoid that reports deal with larger ticks (ex: tick offsets can then be used, instead of absolute simulation ticks), see ``class_Probe:setTickOffset/2`` and the soda vending machines as an example



Version 2.0.10
--------------


Released on Friday, February 4, 2011.


MD5 code of the archive::

  0e19b02fb4446a44536e2890f97b67cb  Sim-Diasca-2.0.10.tar.bz2

Size: 3.9MB.



Most significant changes were:

 - all layers: tests, applications, traces facilities have been improved (ex: no more macros) and integrated

 - in ``Common``:

   - support for smart exceptions has been added (but is currently disabled due to a bug in their parse transform)

   - support for basic stats (ex: file count, line count split in empty/code/comment ratios) added (run ``make stats`` for the Sim-Diasca root)

   - user-defined code paths are transformed now into absolute ones, so that the current directory can be changed while still being able to load newly referenced modules; as a consequence, far longer paths were displayed on the console, thus the verbose mode for the Erlang launcher has been disabled (add '-v' to the ``ERL_PARAMETERIZED_LAUNCHER`` definition in ``common/GNUmakevars.inc`` to restore this behaviour for debugging)


 - in ``Sim-Diasca``:

  - all cluster scripts massively improved, extended and fixed

  - various settings (ex: thread pool size, process limit, inter-tick time-out of the VM, etc.) are now better managed for the computing nodes (their value is automatically set to the one of the user node, thus uniform settings can better be enforced in a simpler and safer way)

  - the deadlock-detection mechanism (triggered in case of simulation stall) has been rewritten, on a purely asynchronous manner; previously it could cause itself transient deadlocks which could increase the effect of simulation stalls, if an actor sent a (blocking) request whereas the time-manager was nudging it (it was a synchronous operation with a time-out)

  - probe data files (either basic or virtual) now embed meta-data in a comment header, detailing notably generation date and time, probe name and curve names

  - probes default size increased from 1024x768 to 1600x1200 for better rendering

  - the user-specified simulation frequency is now better managed (well-checked, absolutely, and the actual one is validated also relatively against the specified one, based on a user-specified tolerance)

  - performance tracker has been improved and integrated to the engine (ex: see the ``enable_performance_tracker`` field in the deployment settings)

  - names have been homogenised, notably ``testFailed/1`` became ``test_failed/1``, ``testFinished/0`` became ``test_finished/0``, etc., and most macros for test or simulation cases are now better named and implemented as functions, whenever possible

  - the host configuration file (ex: ``sim-diasca-host-candidates.txt``) can now support the specification of a per-host user name, notably to support cases where the login of a simulation user depends on the host

  - all computing nodes are now fully set-up and deployed in parallel, rather than one after the other; this is especially useful if having numerous nodes, as detecting them, network-wise, and having them process a deployment archive can last for some time, for each of these nodes; this induced major changes (see newly introduced class_ComputingHostManager); the issue of lingering nodes blocking sometimes some hosts has been fixed as well

  - deployment time-outs are better managed (see the ``maximum_allowed_deployment_duration`` deployment settings)

  - result manager integrated (main feature of this release): now probes (either basic of virtual, i.e. based on the datalogger) are, inheritance-wise, specific result producers that automatically register to the result manager; then, if their outputs are selected as intended results and if the simulation terminates normally, their time series (data) and/or corresponding plots (then generated on purpose) are sent back to the result manager, so that all the results are retrieved automatically on the user node






Versions 2.0.11 & 2.0.12
------------------------

None of these two versions was officially released.

A significant time elapsed between this version and the previous one: many improvements were made.

Most significant changes will be:

 - in general:

   - the ``add-deduced-type-specs.escript`` script has been completed, fixed and applied to the whole Sim-Diasca codebase

   - measures have been taken so that all tests (either at the Common-level with ``test_facilities:start/1``, or at the Trace-level with the ``test_start/0`` macro) are run without trapping exits anymore, which was the default (otherwise some failures could be silent)

   - all main loops of spawned processes are spawned thanks to (correct) closures now, to avoid having to export the corresponding functions

   - two overall execution targets, 'development' (the default) and 'production' are defined now, they allow to configure the whole software stack according to these profiles (ex: in production mode, debug checkings and traces are removed, some operations cannot fail on time-out anymore, etc.); ex: ``make clean all EXECUTION_TARGET=production``; note that this is a compile-time setting, not a runtime one; native compilation is currently enabled in production mode, but may not be in the future, depending on the feedback

   - detailed type specifications have been added everywhere (each function of each module of each layer); process initially partly automated thanks to our ``add-deduced-type-specs.escript`` script

   - all layers have been analyzed thanks to Dialyzer (automated make targets added), and their code has been improved accordingly


 - in ``Common``:

   - support for smart exceptions has been removed (their code was faulty and they were superseded by R15B features)

   - a hashtable feature allowing to perform its automatic static optimization (ensuring its actual load factor is close enough to its ideal one) has been implemented and tested (see ``hashtable:optimise/1``); other kinds of hashtables (``tracked_hashtable`` and ``lazy_hashtable``) have been introduced (experimental support only); other minor features added

   - work-around added to resist to the possible race-condition in the Erlang kernel between the execution of ``rpc`` operations on a remote node and the local registration there of the code server (see ``basic_utils:wait_for_remote_local_registrations_of/3``); our ``install-erlang.sh`` script is now able to patch automatically the Erlang code accordingly


 - in ``WOOPER``:

   - the automatic hashtable tuning has been integrated in WOOPER, it is done on the instance state hashtable as soon as its construction is over (as by convention no new attribute is expected to be defined then), on the class-wide virtual tables, and on the hashtable allowing the class manager to serve the virtual tables of the various classes

   - default synchronous time-outs better defined, depending on debug mode being enabled or not; this debug mode is no more to be specified in ``wooper.hrl``, instead of modifying a source file, one may specify it at the makefile-level (ex: ``make ENABLE_DEBUG=true``)

   - a generic WOOPER proxy instance has been defined, for very specific cases (see ``wooper_instance_proxy.erl``)

   - most error conditions (from any kind of method, or from a constructor or a destructor) now result in a full stack trace and the full state of the instance to be displayed, for easier debugging (with line numbers, starting from R15B)

   - ``remote_synchronisable_new*/N`` operators added, for synchronous (blocking) yet parallel instance creations


 - in ``Traces``:

   - includes, exports and function definitions have been re-arranged to work in all combinations of contexts (ex: from a test with traces, from an application without, with or without batch mode, etc.)

   - now the activation/deactivation of traces is done through makefiles, rather than by changing the source code directly: instead of commenting/decommenting a define in ``class_TraceEmitter.hrl``, one must set ENABLE_TRACES to false if wanting to disable them; in all other cases, they will be activated (beware to typos); for example "make TARGET ENABLE_TRACES=false" will disable them, whereas they will be enabled with "make TARGET ENABLE_TRACES=true" and with "make TARGET"; this trace setting will be kept if this make command is to recurse in other directories

   - there could be a (rather uncommon) race condition between the launchings of the trace aggregator and trace supervisor: if the latter came quicker enough than the former, then the LogMX-based supervisor could try to open a file that was not created yet by the aggregator, or that was created but was still empty; now this optional supervisor requests the aggregator to tell it when the traces are ready to be read, so that LogMX cannot complain anymore

   - now the most critical channels (``Warning``,``Error`` and ``Fatal``) will never be silenced, even if traces are deactivated; they will be additionally always be echoed on the console; trace constructs have been made completely uniform (sent from instance, test, application, etc.)


 - in ``Sim-Diasca``:

  - the data exchange service has been added (see ``class_DataExchanger``), and supports all documented features (one of the main highlights of that version)

   - most hashtables benefited from the load factor optimisation

   - default deployment time-out increased a bit for slower standalone computers (from 5 to 8 seconds), and now is computed depending on the execution target

   - basic probes have been enhanced (zone support, better error management)

   - in addition to the currently supported scheduling possibilities for an actor at a given tick (``none``, ``triggered``, ``spontaneous``, and ``twofold``), ``instant_spontaneous`` is now supported, allowing to dynamically (i.e. during the same tick) convert a triggered tick into a twofold one, i.e. allowing to plan conditionally during the processing of the actor messages a spontaneous tick which was not anticipated and will follow immediately; this is useful for example if an arbitrary number of actor messages of a given type could be received during a tick, whereas having to perform some processing which depends on all of them simultaneously, thus needing to do so only once all have been received for sure (none can appear afterwards); an ``instant_spontaneous`` action will be executed if at least one call to the ``class_Actor:request_instant_spontaneous_action/1`` function is made during the processing of an actor message, in the context of a triggered tick

   - now the result manager, at simulation start, sums up in traces the number, type and names of all selected result producers (ex: basic probes, virtual ones)

   - simulation milestone management, both with regard to wallclock time and to simulation time, has been implemented, allowing to perform all kinds of operations then; currently: some tracking of the memory footprint of simulation agent is performed, as well as some house-keeping, garbage-collector wise

   - a distributed service for instance tracking has been added, so that faulty models can better be fixed

   - a troubleshooting mode has been introduced, to further help model debugging

   - major update of probes, notably to properly support zones (ex: for the performance tracker) and probe types (result or facility)

   - performance tracker cleaned-up and improved

   - we switched to a semantic versioning (see `explanation <http://semver.org/>`_)

   - the simulation tick duration is now directly specified rather than a simulation frequency, for clarity and suppression of a source of rounding error; durations in virtual time can now be transformed into a number of ticks with a user-defined maximum rounding error

   - the creation of initial actors can now be done fully in parallel

   - the assignment of seeds to actors is now done directly at actor creation by the load-balancer, instead of being distributed among time managers: this is both simpler than before and at least as scalable; moreover a change in the number of available computing nodes does not break reproducibility anymore




Version 2.1.0
--------------

Released on Friday, November 23, 2012.

This version introduced a major feature: the "zero time bias scheduling" (also known internally as "Fujification", as a tribute to Dr Fujimoto, author of an interesting book about simulations in discrete time), based on the automatic generation of as many diascas (in-tick logical moments) as needed to resolve causality while remaining in the same simulation tick.

Most significant changes were:

 - in general:

   - all layers have been analyzed thanks to Dialyzer (automated make targets added), and their specifications and code have been improved accordingly

   - more records have been used, to better structure exchanged tuples


 - in ``Common``: many additions; utility functions have been dispatched into more numerous modules (ex: ``list_utils``, ``random_utils``, etc.)


 - in ``WOOPER``: error output has been further improved (ex: attributes sorted alphabetically, their values being truncated if appropriate, etc.)


 - in ``Sim-Diasca``:

   - the "zero time bias" feature has been added, integrated and tested

   - a full load control mechanism for the distributed result generation has been added




Version 2.1.1
--------------

Released on Wednesday, March 20, 2013.

Mostly a bug-fixing version.


Most significant changes were:

 - in general: all mechanisms tested and updated (ex: production mode), full code checked against Dialyzer

 - in ``Common``: various minor additions; in GUI, ``gs`` has been fully replaced by ``wx``

 - in ``Traces``: timestamps now are reported in LogMX as tick offsets rather than absolute ticks, for a far better readability

 - in ``Sim-Diasca``:

   - now the temporary deployment directories created on each computing node have far fewer chances of colliding; for example ``/tmp/sim-diasca-My_Simulation_Case-boudevil`` became now ``/tmp/sim-diasca-My_Simulation_Case-boudevil-2012-12-7-at-13h-56m-03s-1f793a6ba507``, which is a lot safer ; such directories are automatically deleted when results are collected

   - similarly, result directories are more unique now, ex::

	``Sim-Diasca_My_Test-on-2012-12-10-at-10h-05m-31s-by-boudevil-1f793a6ba507``

   - k-crash resistance being prepared

   - various meta-data of interest (that can be enriched by the user) are automatically passed to all kinds of probes, so that they are written in their data files

 - in ``Mock-Simulator``: the full *City-Example* benchmarking case has been added (code and documentation), and tested; this is a fairly involved (ex: see ``class_WasteTruck.erl``), representative and scalable test case that can be shared with Sim-Diasca



Version 2.2.0
-------------

Released on Thursday, June 27, 2013.


 - in general: we switched from SVN to GIT, and rewrote many rules (ex: to generate releases) accordingly, relying on an improved layout

 - in ``Common``:

   - a very fast file sending system has been added (see in ``send_file/2`` and ``receive_file/{1,2,3}`` in ``net_utils``)

   - the way the Erlang VM is launched by the automatic make rules (through ``launch-erl.sh``) has been changed, from running ``erl -eval`` to using ``run_erl``; reason: otherwise the VM would halt as soon as the first exception is triggered (ex: ``noconnection``), while we need it to resist

 - in ``Sim-Diasca``:

   - now the simulation archive is sent using ``sendfile``, i.e. on one of the very fastest ways; the potential issues of network congestion, spurious time-outs, size limit of the archive, efficiency have been considerably alleviated thanks to this newer, out-of-band, transfer

   - the (usually local) directory for temporary information to be used by computing nodes (typically ``/tmp``) can now be set by the user (see the ``temporary_directory`` field of the ``deployment_settings`` record)

   - the full deployment course of activation has been made a lot more modular and tractable

   - k-crash resistance (first, limited version thereof) being implemented



Version 2.2.1
-------------

Internal version only, focusing on improving the k-crash resistance.



Version 2.2.2
-------------

Not realised yet.

 - in ``Mock-Simulators``: the plural is used now (simulatorS), as the example set is expected to grow



Future Versions
---------------

:raw-html:`<img src="xkcd-mu.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-mu.png}`


Next features to come, maybe in that order:

  - WOOPER 2.0 integration (a bit complex, as, first, the WOOPER 2.0 version must be fully satisfying before being integrated; will take some time)

  - integration of more and more complex test cases, to resolve all issues (including large-scale simulations), starting by the soda vending machine test case to a far more involved scalability test (see the 'City Example', in the ``mock-simulators`` directory

  - tune for maximum scalability
