:raw-latex:`\pagebreak`

--------------------------
Sim-Diasca Troubleshooting
--------------------------


First Of All: Did You Read The Manuals?
=======================================

:raw-html:`<img src="xkcd-rtfm.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-rtfm.png}`


Besides this document, a Sim-Diasca user should definitively read the *Sim-Diasca Developer Guide*, which is freely available as well. We are not providing ``man`` pages yet.



Troubleshooting Principles
==========================


First at all, everything is done so that:

 - the simulation crashes as soon as a problem occurs (in the engine, in the models and/or in the simulation case), so that there is not silent error
 - in debug mode, many additional runtime checkings are performed (ex: with regard to actor scheduling, termination, etc.)


As a consequence, models in development will crash early and often, and the diagnosis should be quite easy to obtain, thanks to the detailed crash information being given.

Each actor having its own sequential stream of instructions, sharing no data, relying only on its state variable, exchanging only messages according to well-defined procedures should help a lot the debugging.

So, unlike other approaches like this one:

:raw-html:`<img src="xkcd-compiler_complaint.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-compiler_complaint.png}`


this is a normal and easy process for the model developer to iterate simulation runs again and again (*let is crash* belongs to the Erlang principles), until having complete and correct models.

Of course, the fact that a model does not crash does not necessarily imply it respects its intended behaviour: it is of course part of the work of the model developer to check their implementation against their specification.



Most Common Issues
==================

The most common errors encountered while using Sim-Diasca are explained and solved here. They are roughly sorted by increasing potential order of appearance.


:raw-html:`<img src="xkcd-computer_problems.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-computer_problems.png}`



**Issue #1**: ``Protocol: register error``

	If running a simulation and being notified of a strange error like::

		{error_logger,{{2008,11,25},{15,25,6}}, "Protocol: ~p: register
		error: ~p~n", ["inet_tcp",{{badmatch,{error,duplicate_name}},
		[{inet_tcp_dist,listen,1},{net_kernel,start_protos,4},
		{net_kernel,start_protos,3},{net_kernel,init_node,2},
		{net_kernel,init,1},{gen_server,init_it,6}, {proc_lib,init_p,5}]}]}
		[..]

	It is the symptom that a similarly-named Erlang virtual machine is already running on the same host. Either it is an idle forgotten simulation still opened on another terminal [#]_ (in that case shutting down the corresponding virtual machine will correct the situation), or the user really wants to have two instances of the same simulation run simultaneously. In that case, the two virtual machines should be named differently, knowing that, with the default Sim-Diasca Make rules, the launched virtual machines are named according to the case that is run. For example, to run the simulation case defined in ``simulationAndScenario_test.erl`` from the host ``myhost.example.org``, one may just issue ``make simulationAndScenario_run``. The corresponding Erlang virtual machine will be then named ``simulationAndScenario_run@myhost.example.org``.

.. [#] This can happen if for example you issued ``CTRL-Z`` then put that task in the background (``bg``) and forgot that it was still running.



**Issue #2**: ``Can't set long node name! Please check your configuration``

	This may happen whenever the network configuration of the local host is not consistent, at least from the point of view of the Erlang virtual machine. More specifically, it can happen if in the ``/etc/hosts`` file the first name to appear for the local host is not the expected proper FQDN (*Fully-Qualified Domain Name*).

	For example, a line like::

	  127.0.0.1 localhost.localdomain localhost foo.bar.org foo

	should be corrected into::

	  127.0.0.1 foo.bar.org foo localhost.localdomain localhost



**Issue #3**: Execution seems to be blocked right after having been triggered.

	This may happen if using a virtualized environment (ex: VMWare). This does seem to be a general problem related to timers and message receiving, so Sim-Diasca should not be the culprit here. Erlang might not be guilty either, as issues were reported on the VMWare-side.

	Anyway, because of these problems and the performance penalty, the use of virtualized environments should be avoided here.


**Issue #4**: At start-up, no available computing node is found, each candidate node being apparently successfully launched, but not responding.

	This may happen if a previous simulation crashed and thus could not reach its clean-up phase: then pending Erlang nodes, spawned by the previous run, may linger for up to 10 minutes before their automatic shutdown, should the node cleaner script have been unable to remove them, for any reason (which must be very uncommon).

	Indeed their node name will be correct, so no attempt to launch them will be made, but the automatic authentication system of the engine, based on security cookies generated from a unique UUID, will prevent the connection to these preexisting nodes. They will thus be deemed unavailable and the simulation will stop, short of being able to rely on any computing node. The solutions is then either to remove these pending nodes manually (one effective yet rough means of doing so being ``killall -9 ssh beam beam.smp``, to be run on all computing nodes) or to set the ``perform_initial_node_cleanup`` field in the ``deployment_settings`` record to true (see ``class_DeploymentManager.hrl``), in which case any lingering node would be removed when colliding with a newer run; as this latter setting is the default, this issue should not happen frequently anymore, or at all.



**Issue #5**: A simulation case is launched, yet it freezes just after the line telling the trace aggregator has been created, and stays unresponsive until CTRL-C is entered.

	This typically happens after a first failed launch: a virtual machine bearing the same name is already running on the background, thus preventing another one to be launched. The solution may be as simple as a brutal, yet efficient, ``killall -9 beam.smp``.

	This issue occurs more frequently now that the default launching mode relies on ``run_erl`` (rather than a direct start from the command-line): no more ``{error_logger,T,"Protocol: ~tp: the name X@Ya seems to be in use by another Erlang node",["inet_tcp"]}`` is displayed by the VM (as discussed in issue #1). Strangely enough, the problem may happen during the mass running of tests (ex: when executing ``make test`` from the root). ``run_erl`` is suspected here.



**Issue #6**: At start-up, the rebuild of the simulator codebase fails, although the code is correct.

	This may happen if at least one source file (ex: ``myFile.erl``) is being edited without having been saved yet: some editors then create a temporary file like ``~myFile.erl`` or ``.#myFile.erl`` in the same directory. The make system will try to rebuild that file, but the compilation will fail necessarily, as this filename will not match the module name. A proper error message should have been sent in the simulation traces.



**Issue #7**: Some changes to the source code have been made, yet the newer executions seem to correspond to the code that existed before the change rather than to the updated one. Or, more generally, the executed code does not seem to correspond to the specified one.

   This could happen when multiple BEAM versions of the same module can be found from the deployment root. For example, from some subdirectory in the sources, one may have issued ``cp -r foo_directory foo_directory-hidden``, to save temporarily its content while experimenting in-place in ``foo_directory``.

   The problem is that the deployment manager will scan from the deployment root for all BEAMs, and include them in the deployment archive. As a result, on each computing node, any BEAM found in ``foo_directory-hidden`` will be deployed as well and, depending on the code path, ``foo_directory-hidden/a_module.beam`` will be found before ``foo_directory/a_module.beam`` (and this tends to be often the case). As a consequence, the previous version of the code (the hidden one) will be wrongly executed.

   The solution is to avoid to perform back-ups directly in the source tree or, at the very least, to copy them once all BEAMs have been removed, to avoid they silently collide.



**Issue #8**: My simulation seems to be finished, however it does not return to the shell, and it is still eating a lot of resources for quite long. What's happening?

	It may happen whenever a simulation is executed for a long time and/or with numerous actors, whereas the intensity of trace sendings has not been lowered: although all trace modes write down a trace directly as soon as possible once received, and none, except the PDF mode, incurs long processings at shutdown, nevertheless all trace modes can significantly delay this shutdown phase.

	The reason is that the trace aggregation process (see ``class_TraceAggregator``) could not cope with the speed at which traces are sent by the various emitters, including actors. Thus traces accumulate in the aggregator mailbox, and time is needed for them to be formatted and flushed on disk. Sending too many traces regarding the aggregator speed should be avoided, as accumulating messages in the mailbox may result in a huge RAM consumption, delayed shutdown, and risk that a simulation crash happens whereas the corresponding traces are not written yet.



**Issue #9**: At runtime, an exception like ``{unexpected_ack_from,APid,PidList,ATick,ActorPid}`` is thrown.

   Although it looks as if the engine is faulty, the cause must lie in the code of the class corresponding to the instance ``ActorPid`` refers to: most probably that an updated state was not taken into account into one of its methods, from where an actor message was sent (directly or not, like in the case of the creation of another actor) to the process corresponding to ``APid``.

   Indeed an actor message must have been sent, returning an updated state tracking that sending, whereas a previous state, unaware of that sending, was instead returned to WOOPER by that method. Thus when that actor received the acknowledgement corresponding to the actor message it sent, it does not correspond to any recorded sending, leading to the ``unexpected_ack_from`` exception to be triggered.



**Issue #10**: Simulation runs, but is slow.

   This is a difficult issue to tackle generically. Some slowness are more acceptable than others:

   :raw-html:`<img src="xkcd-long_light.png"></img>`
   :raw-latex:`\includegraphics[scale=6.0]{xkcd-long_light.png}`

   Most efficient solutions to increase speed are:

	 - increase your computing resources (more nodes, more powerful, better network, etc.); check that you are never hitting the swap
	 - make (a better) use of advanced scheduling (models seldom require all the same evaluation frequency)
	 - selectively tune your models (ex: use ``etop`` and the traces to spot the most-demanding ones)
	 - improve your algorithms (ex: choose better data-structures)
	 - switch to more "exotic" solutions, like native compilation or the use of NIF
	 - ultimately, reduce your problem size (if at all possible...)



**Issue #11**: Simulation seems to freeze, or to be surprisingly slow, or more generally does not behave as expected, and I do not want to stick ``io:format`` calls everywhere to understand what is happening

	If not using the simulation traces either to figure out what is happening, then a good approach could be to connect to the busiest computing nodes (use simply ``top`` on each host) to determine what they are doing; to do so, track in the console the line which reminds the user of the names of the computing nodes and of the simulation cookie, like in::

	  To connect to computing nodes [
	   'Scheduling_scalability_test-boudevil@server1',
	   'Scheduling_scalability_test-boudevil@server2',
	   'Scheduling_scalability_test-boudevil@server3'], use cookie
	   '1f793a6ba507-d389-2e11-5bd1-2f759320'.

	Then run a new node, connect to the computing node and run ``etop`` to inspect it, like in (maybe exporting ``DISPLAY`` and/or increasing the net tick time can help)::

	  erl -setcookie '1f793a6ba507-d389-2e11-5bd1-2f759320' -sname inspector
	  (inspector@tesla)1> net_adm:ping(
		'Scheduling_scalability_test-boudevil@server2').
	  pong

	Then hit CTRL-G and enter::

	  --> r 'Scheduling_scalability_test-boudevil@server2'
	  --> j
		1  {shell,start,[init]}
		2* {'Scheduling_scalability_test-boudevil@server2',shell,start,[]}
	  --> c 2
	  (Scheduling_scalability_test-boudevil@server2)1> etop:start().

	(note that the ping is not necessary, just issuing ``r 'Scheduling_scalability_test-boudevil@server2'`` then ``c`` would suffice)

	Then you are able to see something like:

:raw-html:`<img src="etop.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{etop.png}`

	You can also run ``observer`` instead::

	 (Scheduling_scalability_test-boudevil@server2)1> observer:start().

	And then we have:

:raw-html:`<img src="observer.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{observer.png}`




Common Misconceptions
=====================

:raw-html:`<img src="xkcd-misconceptions.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-misconceptions.png}`


Here is the list of most common misconceptions we spotted:


**Traces are part of simulation results**

  This is not what we promote: we see the distributed traces as a way of monitoring technically a simulation run. Results are typically probe reports. Moreover, for actual large-scale runs, we generally prefer to disable traces.


**The Performance Tracker is the one responsible for the progress information output on the terminal**

  No, the culprit is the `console tracker`_, which is a live lightweight Sim-Diasca built-in, whereas the `performance tracker`_ is an unrelated, optional, more complex post-mortem feature.
