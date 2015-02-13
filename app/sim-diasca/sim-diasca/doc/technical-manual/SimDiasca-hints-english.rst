:raw-latex:`\pagebreak`

----------------
Sim-Diasca Hints
----------------

Some general hints are detailed here.

:raw-html:`<img src="xkcd-wisdom_of_the_ancients.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-wisdom_of_the_ancients.png}`



Common Pitfalls
===============

 - when an actor instance is created from another one, it should be created *synchronously*, through the load balancer; the creator should wait for the creation to be notified, otherwise for example the created instance *could* subscribe on the next tick instead of on the current one

 - communication between actors should *only* rely on the exchange of actor messages (no basic WOOPER method invocation or direct Erlang message sending allowed)

 - to reduce the number of messages exchanged over the network and more generally to increase speed, trace sending is asynchronous (i.e. non-blocking). Thus if ever a simulation happens to send way too many traces (actors too verbose), then:

	- the trace aggregator will most probably lag behind, i.e. the traces being monitored in the course of the simulation will not correspond to the current state of the execution, but to a prior simulation state; and, should a crash occur, the corresponding traces may never be processed and thus be permanently lost

	- in worst talkative cases, the mailbox of the trace aggregator will lead to a huge memory footprint, possibly resulting in a crash in the course of the simulation (ex: with a message like ``eheap_alloc: Cannot allocate XXX bytes of memory (of type "old_heap")``)

	- the simulation can be finished whereas the simulation case itself is still lingering (not terminating), waiting for the aggregator to process and store all remaining traces (it can last, in worst cases, for hours!)

 - when writing a simulation case (ex: a test case like ``foo_test.erl``), one must know that if the traces and their supervision are activated then, as soon as the user closes the supervision tool (ex: LogMX or the PDF viewer), a ``{wooper_result,monitor_ok}`` message is sent back to the test, so that it is able to finish and shut down the Erlang node; however it means that any receive clause placed in the case (ex: between the calls of the ``test_start/test_stop`` macros) must be written with special care, lest it catches this trace supervision message; for example if the supervision tool is close before a receive like ``BarPid ! {getBaz,[],self()}, receive {wooper_result,MyBaz} -> ... end`` is executed, then ``MyBaz`` will be actually ``monitor_ok`` instead of the expected return value; the proper way of managing that is thus to add, to all receive clauses in the case, a proper guard, like: ``when MyBaz /= monitor_ok``; alternatively, if no other specific guard was needed in a receive clause, then it can be replaced by ``MyBaz = test_receive()`` instead (defined in ``traces_for_tests.hrl``), as it enforces that the received result is not ``monitor_ok``

 - if a specialized actor class ``class_Foobar`` overrides the ``simulationStarted/3`` method, a special care must be taken when writing its constructor (``class_Foobar:construct``). Indeed the call to the overridden ``simulationStarted/3`` method may happen directly from this constructor, through the call to the constructor of the (direct or not) mother class ``class_Actor``, should the corresponding actor be created whereas the simulation is already started (non-initial actor); as a result, one must ensure that any attribute modified in ``class_Foobar:simulationStarted/3`` (ex: ``baz`` set to ``true``) is not overwritten later in the class-specific part of ``class_Foobar:construct``, in what could be incorrectly considered as the initial setting of this attribute (ex: ``baz`` set to ``undefined``). See ``class_TestActor`` for an actual example of the dealing with that constraint






Good Practices
==============

 - all Erlang good practises should be followed

 - all WOOPER good practises should be followed

 - always think to the life-cycle of the actor instances you create: when are they to be created? By whom? When they shall be deleted?

 - in a simulation, either an actor is created before the simulation start, or by another actor; quite soon everything is driven by a model (ex: the deployment policy of devices in a simulated system should be an actor of its own)

 - each model should better be documented into a wiki page of its own



Lesser-Known Features
=====================


:raw-html:`<img src="xkcd-nine.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-nine.png}`

One should be aware that:

 - even if the most usual mode of operation for SimDiasca-based simulators is the *batch* mode, the engine can also work in **interactive** mode as well, where the simulation is kept on par with the wallclock time (rather than running as fast as possible)

 - by default, the engine works in reproducible mode, based on a constant random seed, leading to always the same simulation trajectory for a simulation case; the engine can also work on (reproducible) **ergodic** mode, in which it changes the random seed at each simulation run, so that all the various possible trajectories can be explored, instead of just an arbitrary one

 - by default, probes write their results onto raw files; a database-based back-end is available as well, see the **Data-Logger** module for that

 - the engine includes a performance tracker, a service that can be enabled to track the behaviour of a simulation over both wall-clock and virtual time, and also its detailed resource consumption



Other Useful Information
========================

 - a WOOPER-aware ``Nedit`` Erlang configuration file is available (see ``common/conf/nedit.rc``)

 - all Sim-Diasca Erlang source files (``.hrl/.erl``) should start with the appropriate LGPL header defined in ``sim-diasca/doc/licence/licence-header-erlang.txt``


 - the used Erlang environment should better be built thanks to a shell script we provide, ``common/conf/install-erlang.sh``, to streamline this process; use for example ``common/conf/install-erlang.sh --cutting-edge --doc-install``; add the ``--generate-plt`` option if intending to make any actual development in the future

 - in the cases where LogMX cannot be used to monitor the simulation traces, a fall-back system can be chosen instead: traces can be output as a human-readable text file which can be read by any text viewer; to do so, one just has to edit the ``sim-diasca/src/core/src/test_constructs.hrl`` file, in which ``-define(TraceType,log_mx_traces).`` should be replaced by ``-define(TraceType,text_traces).``

 - Sim-Diasca is able to run on multiple computing hosts, possibly with different user names; these hosts, and per-host user names as well, can be specified thanks to the ``computing_hosts`` field of the ``deployment_settings`` record (see ``class_DeploymentManager.hrl``)

 - where is the temporary data for the simulation stored? The default value of the ``temporary_directory`` field of the ``deployment_settings`` record is ``/tmp``; hence temporary data for a simulation case named ``Foo`` run by a user ``norris`` would be stored, on each host, in, for example, ``/tmp/sim-diasca-Foo-norris/2013-6-5-at-10h-38m-17s-1de19ec70ed5`` (the suffix is made of a wall-clock timestamp and a rather unique simulation ID); on simulation success, this directory will be automatically removed

 - how is this temporary data organised? In the general case, there are three top-level directories:

  - ``deployed-elements``, which contains the simulation archive (typically ``Sim-Diasca-deployment-archive.sdar``) and the extracted trees thereof (typically with the main simulator layers, like ``common``, ``wooper``, ``traces``, etc.)

  - ``outputs``, where simulation probes write their files (``*.dat`` for data, ``*.p`` for the corresponding commands); as for technical probes (ex: for the performance tracker), they are directly written in the final result directory, as they must remain available in all cases (even if the simulation crashed)

  - ``resilience-snapshots``, where the persistance files for each secured node are stored, based on the tick and diasca of the serialisation and the node on which it was done (ex: ``serialisation-5719-0-from-cluster-node-147.example.org``)

 - what are the constraints applying to the name of an attribute? Such a name must be an atom, and all names starting with ``wooper_``, ``traces_`` or ``sim_diasca_`` are reserved, and thus shall not be used



Tips And Tricks
===============

 - when running a simulation across multiple hosts, different versions of the Erlang runtime may coexist; if these releases are too distant in time to be compatible, the problem will be detected by Sim-Diasca and the incompatible versions will be reported; in this case one generally needs to install, out of the system tree, a newer version of the runtime to replace the oldest versions (use for that our ``install-erlang.sh`` script; more generally speaking, all Erlang runtimes *should* stick to the latest stable version, to benefit from the latest improvements); however, for these environments overridden by the user to be found by Sim-Diasca, they must become the default ones for that user; adding a line like ``PATH=~/my-install/bin:$PATH`` in one's shell settings (ex: ``~/.bashrc``) is necessary but not always sufficient, as remote SSH login may not lead to that file being sourced; one should just check that on the target hosts the expected version Erlang version is used (ex: ``ssh USER@HOST erl`` allows to check the version); typically, with the ``bash`` shell, the ``.bash_profile`` file should contain something like: ``if [ -f ~/.bashrc ]; then . ~/.bashrc ; fi``

 - when adding a source file to the Sim-Diasca engine, use the ``add-header-to-files.sh`` script with an appropriate header, for example::

   $ add-header-to-files.sh ../licence-header-erlang.txt MyNewFile.erl

 - one may define in one's shell settings (ex: ``~/.bashrc``) a variable that disables the automatic launch of the various windows (ex: LogMX interface, result browser, etc.), like in::

   export BATCH="CMD_LINE_OPT='--batch'"

then running a test as ``make my_test_run $BATCH`` will prevent any Sim-Diasca related window to pop up; this is quicker and more convenient when first debugging a new model: we generally have to focus first on runtime errors on the console. Then, only when these first mistakes are corrected, we can take advantage of the simulation traces and other information (with the usual ``make my_test_run``)

 - one may also define in one's shell settings (ex: ``~/.bashrc``) an alias that points to the current check-out (clone) and branch one's is using: otherwise an absent-minded developer could operate directly in the trunk or in a wrong branch; for example one can use: ``alias tosim='cd $HOME/A_PATH`` (with GIT reusing lastly used branch is less a problem)

 - simulation traces can be inspected without LogMX, see the `Simulation Traces`_ section

 - sometimes, in error messages, we can see weird lists like::

  ``[84,104,105,115,32,105,115,32,97,32,115,116,114,105,110,103,46]``.

  they are actually strings, that can be properly displayed by pasting them in an interpreter::

   1> [84,104,105,115,32,105,115,32,97,32,115,116,114,105,110,103,46].
   "This is a string."

 - knowing that the simulation engine relies on reproducible AAI, no special effort is made so that PID are themselves reproducible; moreover, notably in a distributed context, reproducibility of PID *cannot* be ensured at all (ex: two actors may create another actor each during the same tick); however, to investigate the mode of operation of the engine, it is convenient, as least for the first few simulation phases, to try to reduce the PID variability from a run to another, so that the same agent (ex: the load balancer) always bears the same PID; the simultaneous launching of the LogMX interface tends to make the first PID change a lot (ex: ``<x.52.0>``, then ``<x.58.0>``, then``<x.56.0>``, etc.); to reduce this trend, one should preferably run the simulation in batch mode: PID will then be a lot less changing; for example: ``make my_case_run CMD_LINE_OPT="--batch"``

 - sometimes one may want to connect to the running Erlang VM, in order to determine what is happening there; to do so, one should note the pipe this VM is attached to (for that one should refer to the console output: one of the very first lines is akin to ``Attaching to /tmp/launch-erl-4938 (^D to exit)``; then executing from another terminal ``to_erl -F /tmp/launch-erl-4938`` allows to connect to the VM

 - in case of a failure during a simulation, some Erlang nodes may linger on various computing hosts and be on the way of the next run; to ensure each new run cleans up any lingering node before launching a simulation, one may set the ``perform_initial_node_cleanup`` field in the ``deployment_settings`` record to true (see ``class_DeploymentManager.hrl``). Then another step will be added to the simulation start (which thus will take a bit longer), but no new run will have to reject a computing host because of an already existing node running with the target name but a different cookie; in all cases, a simulation cannot use such nodes by mistake, thanks to the unique cookie it generates at each launch

 - one may use the ``common/src/fix-all-sources.sh`` script periodically (from fully check-ined sources) to clean-up sources and remove unbreakable spaces

 - in some cases, mostly related to probe storage or post-processing, for example if wanting to create a large number of basic probes using immediate (non-deferred) writes (which is the default), you may be hindered by the maximum number of open file descriptors, which is usually set to 1024, thereby limiting the number of basic probes to, roughly, a thousand per computing node; one can use ``ulimit -n 20000`` to set the maximum number of open file descriptors to 20,000, or modify the ``nofile`` item in ``/etc/security/limits.conf`` file; as both operations require root privileges on most systems, this is not managed by Sim-Diasca

 - on clusters, notably with PBS-based clusters, output log files (standard and error, ex: ``Sim-Diasca.o1983473`` and ``Sim-Diasca.e1983473``) will be available *only* once the simulation is terminated (on error or on success); however, for most computations, notably the ones with high maximum durations, knowing whether the simulation is making relevant progress, or just wasting resources due to any issue, is surely convenient, as it allows either to monitor the corresponding task or to kill it a lot earlier, freeing the corresponding resources; to access this information, one has to connect to the node from which the simulation was actually run from by the job manager; this involves getting the job identifier (ex: thanks to ``qstat -u $USER``), determining the first allocated node (ex: ``qstat -f 1983473.cla11pno | grep exec_host``), connecting to it (directly with ``ssh`` rather than with ``qsub -I``) and look at ``/var/spool/torque/spool/${job_id}.OU``, ex: ``/var/spool/torque/spool/1983473.cla11pno.OU``
