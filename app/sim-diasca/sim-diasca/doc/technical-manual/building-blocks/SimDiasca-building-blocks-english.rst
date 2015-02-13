:raw-latex:`\pagebreak`


--------------------------
Sim-Diasca Building Blocks
--------------------------


.. _traces:
.. _simulation traces:



Simulation Traces
=================



Principles
----------

Traces (a.k.a. simulation logs) are a way of recording for later use the events happening during a simulation. There are not simulation results per se, their purpose is to make the technical troubleshooting easier, notably to help developing and debugging models.

Trace facilities are gathered in a separate layer, the ``Traces`` one (in the ``traces`` directory), which is used notably by Sim-Diasca. The ``Traces`` service depends only on ``WOOPER`` and on ``Common``.

Refer to the *Sim-Diasca Developer Guide* for information about the actual (practical) use of traces.

Defining a trace format allows to uncouple the execution of a simulation from the interpretation of what happened during the course of action (we can either monitor the simulation "live", or study it "post-mortem", after its termination).

If moreover the format is designed to be "universal" (in the context of discrete-time simulations), in order to be independent from a particular domain from any trace generator or supervisor, then the post-processing toolchain of traces might be shared between several simulators.

Traces allow to monitor an execution, without recording each and every event that occurred.



Architecture
------------

With Sim-Diasca, the management of distributed traces is split into the following roles:

- each simulated object (model instance, i.e. actor), but also each technical agent or simulation case, may be led to send traces regarding its operation. The vast majority of them are ``TrameEmitter`` instances (they inherit from that class, directly or not), other are not instances thereof but nevertheless need to be able to output traces (ex: test cases)

- the distributed traces have to be collected and aggregated, this is done by a ``TraceAggregator`` instance, which supports various output formats. Then overall analysis and search for event correlations are possible

- the simulation user might want to follow the life of the system, based on the emitted traces. This can be done either at execution-time (in real time) or post-mortem (once the simulation is over), both thanks to a ``TraceSupervisor``, which can run from a remote host; moreover ``TraceListener`` instances can be created, so that from any host one can connect to the simulation while it is running, catching up automatically (past traces being sent as a whole initially, in a compressed form, next ones being sent directly, so that none is lacking)



How to manage trace-induced overhead
------------------------------------

Sending, collecting and aggregating traces proves very useful, but this feature demands resources, in terms of processing, memory and bandwidth: most of our models and agents by default emit many traces (they are intentionally very talkative), to help troubleshooting. As this is a distributed trace system, traces are emitted concurrently but at the end must be aggregated sequentially, in order to be consulted as a whole, in a single location. Therefore, beyond some simulation scale - and despite careful design - the trace aggregator is bound to become a bottleneck.

As a consequence, the point has been, for non-critical channels, to be able to disable trace sending as a whole, *without any performance penalty* compared to the same code in which there would not be any trace sending at all.

To disable trace sending and incur no runtime overhead, one should ensure that the compile-time make variable ``ENABLE_TRACES`` is set to false [#]_. To do so, one should either specify it directly on the command-line (ex: ``make clean all ENABLE_TRACES=false``), or update directly the make files (typically by setting ``ENABLE_TRACES := false`` in ``traces/GNUmakevars.inc``). Then everything above WOOPER (i.e. the Traces layer, the Sim-Diasca layer, and the code using it) should be rebuilt, as it is a compile-time (as opposed to runtime) option.

.. [#] This will result in *not* having the  ``TracingActivated`` preprocessor symbol defined.


.. Tip::

  If wanting to operate a selection on the classes whose instances must be able to send traces while others cannot, one may just keep the default ``ENABLE_TRACES := true`` and compile everything with traces disabled (from the root: ``make clean all ENABLE_TRACES=false``, then  update the timestamp of the source files of the cherry-picked classes that are to be allowed to send traces (ex: ``touch class_Foobar.erl``), then run ``make all`` from the root of the sources: all classes then rebuilt will have traces enabled.

  Of course this can be done the other way round, in order to just select the classes that are to be silenced.


Finally, one should know that, if going for higher simulation scales, traces shall not be the only feature to be appropriately managed.




Trace Channels
--------------

There are six built-in trace channels, of increasing importance:

 - ``debug``
 - ``trace``
 - ``info``
 - ``warning``
 - ``error``
 - ``fatal``

Depending on the nature of its message, a trace emitter can select in which channel its current trace should be output.

The three most critical trace channels (``warning``, ``error`` and ``fatal``) will always echo their messages in the console as well (to ensure none can remain unnoticed), and will not be disabled even if the trace system is deactivated (i.e. regardless of the ``ENABLE_TRACES`` settings).


Depending on the needs of the simulation user, various types of trace outputs can be selected:

 - traces integrated to an interactive graphical tool (LogMX)
 - traces in raw text (to be browsed thanks to any text editor)
 - PDF traces (any PDF viewer can then be used)





Traces For LogMX
----------------

The resulting interface when browsing the traces (written in a ``*.traces`` file) with default aggregator output corresponds to:

:raw-html:`<img src="logmx-interface.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{logmx-interface.png}`


Using the LogMX trace supervisor is surely the most useful way of monitoring the traces, in real-time or post-mortem.

LogMX offers rich features that allow to:

 - browse conveniently even large sets of traces
 - select the minimum level of detail for the traces to be displayed
 - search traces for specific patterns, or by emitter, date, location, etc.


The only drawbacks of this trace mode are that:

 - this requires a specific tool (LogMX), properly obtained, installed and configured with the Sim-Diasca parser

 - the results cannot be directly integrated into a report


LogMX-based traces are the default output type. Should the trace output type have been changed, they can be restored by setting ``TraceType`` to ``log_mx_traces``, in ``traces/src/traces.hrl``.



Text Based Traces
-----------------

Simulation traces can be inspected without LogMX, directly from any text viewer, although it is generally less convenient.

To do so, in ``traces/src/traces.hrl``, just define ``TraceType`` to ``{text_traces,text_only}`` instead of ``log_mx_traces``, and rebuild everything above WOOPER (i.e. the Trace package, the Sim-Diasca package, and the code using it).


As a result, simulation traces output in the corresponding trace file (ex: ``soda_deterministic_integration_test.traces``) will be in plain text and encoded for human readability (within a nice ASCII-art array); as a consequence they can read with any text viewer.

By default ``gedit`` will be automatically triggered as a trace supervisor, and the user will be requested by the editor to reload that document as newer traces are appended.

If wanting to use another text viewer, just update accordingly the ``executable_utils:get_default_wide_text_viewer/1`` function in the ``Common`` package (in ``common/src/utils/executable_utils.erl``): ``gedit`` might be replaced for example by ``nedit`` or by ``xemacs``.

Note that only the most interesting trace fields are kept here, for the sake of readability.


.. comment Another option is to define ``TraceType`` to ``{text_traces,direct_text}``, if wanting to have traces be directly written to disk (as pure yet human-readable text), thus without deferred writing. This is the only mode not buffering traces in RAM in the course of execution, thus with no problem of memory footprint or crash before the trace aggregation.



PDF Trace Report
----------------

When wanting to generate a trace report once the simulation is over rather than monitoring the traces during the simulation, a PDF output can be selected.

To do so, in ``traces/src/traces.hrl``, just define ``TraceType`` to ``{text_traces,pdf}`` instead of ``log_mx_traces``, and rebuild everything above WOOPER (i.e. the Trace package, the Sim-Diasca package, and the code using it).

Then, as soon as the simulation has stopped, traces will be properly aggregated and formatted from Sim-Diasca, a PDF being generated and then automatically displayed. This PDF can be useful to share simulation results with remote colleagues.

Only the most interesting trace fields are kept here, for the sake of readability.

Note that this feature implies of course that you have already the proper documentation toolchain installed, which includes the RST converters (``python-docutils`` package) and a PDF viewer (by default, ``evince``).

In the future, we could imagine an enhancement which would allow to convert a trace file generated for LogMX into a PDF, so that the user can generate a report without having to run again the simulation with different trace settings.

However, as simulations are expected to be totally reproducible, it would just a matter of saving some time, so the priority of this enhancement is low.

If wanting to use another PDF reader than ``evince``, just update accordingly the ``executable_utils:get_default_pdf_viewer/0`` function in the ``Common`` package (in ``common/src/utils/executable_utils.erl``): ``evince`` might be replaced for example by ``acroread`` or by ``xpdf``.

Then recompiling this module should be enough.





:raw-latex:`\pagebreak`

.. _probe:
.. _probes:


Probes
======

They can collect all kinds of numerical data produced during simulation, and generate a graphical view of them.

Probes are *result producers*, and as such are dealt with by the *result manager*.

There are two main kinds of probes:

 - *basic probes*, the most resource-efficient, scalable ones
 - *virtual probes*, based on the data-logger, the most flexible and powerful ones


They are based on similar interfaces and share most features, including the management of their rendering.


Among the common features:

 - a probe will be created if and only if it is to produce a result of interest for the simulation, i.e. iff it matches the result specification chosen by the user (in the simulation settings); otherwise this probe will be not created at all, sparing 100% of the resources it would require

 - their data files for time series all start with an header which lists meta-data like generation date and time, probe name, title, and the description of the curves involved



Generic Probe
-------------

A generic probe can be dynamically configured to gather any number of time series, and represent them with as many curves whose abscissa axis corresponds to the simulation time, like in:

:raw-html:`<img src="xkcd-stove_ownership.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-stove_ownership.png}`


Samples *must* be sent to the probe in chronological order (indeed, depending on their settings, they may write directly their data to disk).

An example of the output rendering is:

:raw-html:`<img src="Generic_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{Generic_probe_example.png}`

The generic probe is defined in ``class_Probe.erl``, and tested in ``probe_rendering_test.erl`` (unit rendering test) and ``class_Probe_test.erl`` (integration test).


By default:

 - a probe will create a command file only when requested to generate a report (i.e. not when itself being created, in order to be able to take into account any later change in the curve declarations and rendering options before rendering is requested)

 - the sample data it will receive will be written to disk on-the-fly (i.e. will *not* be cached in memory, to minimize the memory footprint of the probe)


Both behaviours can be changed, thanks to construction-time options (see, respectively, the ``create_command_file_initially`` and ``deferred_data_writes`` options).

As shown in the probe test, new curves can be dynamically declared, so that samples of increasing sizes can be sent over time (in the example, the fourth curve, the purple one, is dynamically added). The corresponding curves will be appropriately rendered the next time a report generation is requested. Existing curves can be renamed as well on-the-fly.

Full time-steps can be skipped, i.e. a new sample might be associated to a tick more than one tick after the previous one, and curve rendering will respect that time lapse (in the example, no sample at all was sent for ticks #8 and #9).

Partial samples can be sent as well, when no data is available for a given curve at a given tick (in the example, the second curve, the green one, had no relevant data for tick #3).


Finally, probes used to support the generation of histograms like this one:

:raw-html:`<img src="xkcd-11th_grade.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-11th_grade.png}`

However this feature is currently deprecated, as synthetic indicators should preferably be generated in a later separated post-processing stage.



Specialised Probes
------------------

.. _reliability probe:
.. _reliability probes:


Reliability Probes
...................

Once associated with an equipment, a reliability probe, still based on a time series, records at each simulation tick the status of this equipment (functional or in failure), and can generate the chronology of status changes as shown here:

:raw-html:`<img src="Reliability_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{Reliability_probe_example.png}`

The reliability probe is defined in ``class_ReliabilityProbe.hrl``, and tested in ``class_ReliabilityProbe.erl``.




Probe Troubleshooting
---------------------

Here are some general troubleshooting hints about probes, when they seem to fail to produce their expected graph renderings.

First, no error nor warning should be output in the terminal or in the simulation traces; otherwise the user code must be fixed accordingly. It can happen for example when no data at all was sent to a probe, whereas it was requested to generate a report.

For each probe which was created, fed with data *and* requested to output, a report (a graphical view of its data) should be made available to the user.

By default, a basic probe will perform immediate (non-deferred) writes: otherwise it would have to keep its sample in-memory, potentially exhausting the RAM if the simulation is long enough.

As a consequence in this case one open file descriptor is used per probe. This limits by default the maximum number of simultaneously existing basic probes per computing node to roughly one thousand.

This limit can be overcome, for example ``ulimit -n 20000``) will raise the maximum number of open file descriptors from the usual default ``1024`` to ``20000``.

This is not done automatically by Sim-Diasca as on most systems this requires root privileges.



Probe Report Generation Issues
..............................

You must first check that you sent at least one data sample to your probe, and then that it is included in the result specification.

If, when the generation takes place, a warning about ``bmargin`` is issued, then you must be using a *very* old version of ``gnuplot`` (ex: 4.0).

In this case you can either:

 - update your ``gnuplot`` version
 - in ``sim-diasca/src/core/src/probe/class_Probe.erl``, disable all key options: set the ``key_options`` attribute to an empty string


More generally, if a problems occurs, you should check first that the probe report (ex: ``Test_probe.png``) was indeed generated.

If yes, it must be a (minor) problem of the display tool, see the ``Probe Report Display Issues``_ section.

If no, actually the PNG could not be generated. The next step is to check in your result directory that the data file (ex: ``Test_probe.dat``) and the command file (ex: ``Test_probe.p``) are correct, i.e. existing, not empty, and not corrupted.

If these files seem correct, you can just check that you can generate the lacking PNG by yourself, by issuing on the command line::

  gnuplot Test_probe.dat

Depending on the outcome (the PNG is generated, or a ``gnuplot`` error is raised), the diagnosis should be easy to determine. Please report to us if a problem remains.




Probe Report Display Issues
...........................

Depending on the tool you use for image displaying, multiple graphs may be displayed in the same window: with the default one, *Geeqie* (``geeqie``, previously known as ``gqview``), one window may pop up, listing all the graphical results.

If wanting to use another image viewer, you can just edit the ``common/src/executable_utils.erl`` file, and update accordingly the ``get_default_image_browser/1`` function.




:raw-latex:`\pagebreak`

.. _datalogger:
.. _data-logger:


Data-Logger
===========

The purpose of the data-logger is to store and manage simulation data in a more powerful and flexible way than with plain probes_.

The data-logger allows any number of *virtual probes* to be created, and stores them in a distributed database. This allows queries to be done on these data, and outputs to be generated in a very similar way as when using plain probes.

This data service relies a lot on the *Mnesia* soft-realtime database, which is an Erlang built-in.

One of the objectives of the data-logger is also to allow for an increased scalability: whereas there is a hard limit to the number of co-existing plain probes on any given computing host [#]_, the underlying distributed tables of the data-logger allow to go one step further, scalability-wise.

.. [#] Indeed, should probes keep their data in-memory only, their size would grow over simulation time until exhausting the system RAM. Therefore they store by default their data in a probe-specific file instead, but then the maximal number of per-process open file descriptors will kick in, and usually will limit the number of plain probes per simulation node to around one thousand, which may not be sufficient.

Moreover some post-processing across tables can then be done more easily.


The data-logger is defined in the ``sim-diasca/src/core/src/data-management/data-storage`` directory.

When running for example the data-logger test (``datalogging_test.erl``, thanks to ``make datalogging_run``), one can see:

:raw-html:`<img src="datalogger-example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{datalogger-example.png}`

In the stacking of windows, from the upper one to the lower one we can see:

 - a rendering of a virtual probe
 - the list of all known virtual probes
 - the database table corresponding a virtual probe
 - a bit of the console tracking





:raw-latex:`\pagebreak`

.. _`console tracker`:


Console Tracker
===============

The *Console Tracker* is a lightweight Sim-Diasca built-in which displays live runtime information on a terminal, in order to allow the user to monitor concisely the progress of a running simulation.

A typical output is::

 Simulation started at simulation time: 1/1/2000 0:00:00
 (tick 3155695199999), real time: 4/12/2010 0:03:31
 with a simulation frequency of 50.00 Hz (period of 20 ms).
 Simulation will stop no later than tick 3155695260000
 (i.e. after 60000 ticks).

 Meaning of the console tracker columns:
 - S: overall [S]imulation time (full time and date)
 - T: overall simulation [T]ick
 - R: [R]eal (wall-clock) time
 - A: total (distributed) [A]ctor count (load balancer included)
 - D: [D]etailed last-tick actor actions
 (spontaneous/triggered/twofold counts)
 - P: total (distributed) [P]rocess count


Then the tick table [#]_:

 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 | Simulation Time      | Tick Offset    | Real Time            | Actor Count  | Detailed Actions             | Process Count  |
 +======================+================+======================+==============+==============================+================+
 |S:       (not started)|T:             0|R:  3/12/2010  2:35:54|A:          0 |D:        0|       0|       0 |P:           64 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:             0|R:  3/12/2010  2:35:54|A:     900503 |D:        0|       0|       0 |P:           64 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:             1|R:  3/12/2010  2:35:55|A:     900503 |D:   900503|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:             3|R:  3/12/2010  2:35:56|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:             6|R:  3/12/2010  2:35:57|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:             9|R:  3/12/2010  2:35:58|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:00:00|T:            12|R:  3/12/2010  2:35:59|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:20:00|T:         59998|R:  4/12/2010  9:39:00|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+
 |S:   1/1/2000  0:20:00|T:         59999|R:  4/12/2010  9:39:00|A:     900503 |D:   900502|       0|       0 |P:       902626 |
 +----------------------+----------------+----------------------+--------------+------------------------------+----------------+

.. [#] The table has been pretty-printed and considerably shorten.

Finally the simulation summary::

 In batch mode, no browsing of results performed. Waiting for their
 processing and retrieval.
 Results are available now.
 Simulation stopped at simulation time: 1/1/2000 0:20:00
 (tick 3155695260000), real time: 3/12/2010 9:39:01, after a
 duration of 20 minutes in simulation time (60000 ticks),
 computed during a wall-clock duration of 7 hours, 3 minutes,
 33 seconds and 22 milliseconds.
 Simulation ran slower than the clock, with an acceleration factor
 of x0.047.



The console tracker is notified of all ticks (and is given the corresponding information), but it selects for display only up to one tick information per second (the latest available), in order not to overwhelm the console and slow-down very fast simulations (thus, each line is a snapshot for a specific tick, not a cumulated value since the last output).

So a simulation run twice will probably display different ticks, thus different information, and this is normal.


One can change the console tracker behaviour so that all ticks are displayed. In this case the tick information from a simulation run twice should match. If it is not the case, there must be a bug, either in Sim-Diasca or in the models.

To switch from one console line per second to all lines being displayed, just edit, in ``sim-diasca/src/core/src/scheduling/class_TimeManager.erl``, the ``tick_tracker_main_loop/3`` function, apply the in-code comments (the ones starting with ``(uncomment next line``), and recompile this module.






:raw-latex:`\pagebreak`

.. _`data exchanger`:



Data Exchanger
==============

The *Data Exchanger* is a distributed simulation service which allows to share conveniently and efficiently data among all actors, even if the data sets are large and the actors numerous.



Objectives
----------

More precisely, the data-exchanger has two technical purposes:

 - to allow for a more efficient, yet safe, sharing of data, based on an automatic distribution which allows all read requests (potentially very numerous) to be performed locally only (i.e. directly on the computing node on which each reading actor is running, thus with very low overhead)

 - to provide a way of propagating data conveniently and as quickly as possible in simulation time, latency-wise: an update made at tick T will be visible to all readers during the full T+1 tick, right from its start, and thus can then be accessed with zero-latency, through any number of direct reading messages per actor (without having to be synchronized thanks to actor messages that would be reordered and executed later); actor-wise, this allows to perform series of any number of (potentially conditional) read requests during the same tick, with potentially arbitrarily complex read patterns, at virtually no performance cost nor developing effort


Each data that is shared according to following conventions:

 - data is defined as key/value pairs, respectively an atom and any Erlang term

 - data can be *static*, i.e. be defined initially and thus be permanently available (ex: through the built-in support for reading from configuration files) and/or be introduced in the course of the simulation, i.e. be *dynamic*

 - can be modified over time once defined (non-const, to be defined with the ``mutable`` qualifier) or not (immutable, to be defined with the ``const`` qualifier)

Note that static/dynamic and mutable/const are orthogonal properties, all combinations of which making sense.

So, basically, compared to inter-actor messages, the data-exchanger offers an alternate, radically different paradigm to manage data, that can be seen as a DSM (i.e. a *Distributed Shared Memory*), somewhat a dual approach to the in-tick reordered actor messages. The point is that this service still allows to preserve simulation properties, and that the different trade-offs it offers may, depending on the situation, be more suitable for some needs of information sharings, notably in the *one writer/many readers* case.

These services had to be offered with care, as, for the model developer, unwanted side-effects or race conditions should not be made easier to perform, and for sure these sharing services must not become a means of bypassing the simulation mechanisms (ex: if instantaneous reads and writes were allowed, then the induced possible immediate inter-actor communication would break at least reproducibility).

Note that the only standard way of exchanging information and synchronising between actors (as opposed to the sharing of information) remains the sending of actor messages.




Sharing Constant Data
---------------------

One of the notable use cases of the data exchange service is the sharing of simulation-specific configuration settings: then all actors can have access to the same static (``const``) data sets, which will be available from the start, before the simulation is started, even before the first initial actor is created. It is then functionally equivalent to having each actor read these information from a common configuration file, except that the reading is done once for all actors on each node, instead of once per actor, avoiding the massive reading and parsing, etc. that would be incurred otherwise.


:raw-html:`<img src="xkcd-x11.png"></img>`
:raw-latex:`\includegraphics[scale=0.8]{xkcd-x11.png}`


These configuration files must respect the Erlang term syntax, with a ``{Key,Value}`` pair or a ``{Key,Value,Qualifier}`` triplet per logical line of the file, ending with a dot. ``Qualifier`` is either ``mutable`` or ``const``; not specifying a qualifier implies ``const`` [#]_

.. [#]  And modifying afterwards a (thus mutable) data without specifying a qualifier will make it ``const`` then.



Such configuration files are to be declared in the ``enable_data_exchanger`` field of the deployment settings (see the detailed syntax in ``class_DeploymentManager.hrl``). They will be then automatically deployed [#]_ and their corresponding data defined. As a convention, their recommended extension is ``.cfg`` (ex: ``my_simulation_parameters.cfg``). See the ``valid_example_configuration_file.cfg`` and ``invalid_example_configuration_file.cfg`` as examples.

.. [#] We preferred adding automatically these user-specified configuration files to the deployment archive and having them parsed once per computing node, rather than read only once and then be sent as messages over the network: for fairly large data-sets, we believe that being compressed in a data archive rather than being expanded as Erlang terms on the sender-side should result in better performances, network-wise.


Of course the data-exchange distributed repository can also be fed at runtime, as opposed to statically (i.e. through configuration files). And this can be done initially (before the simulation is started), through method calls (see the ``define_initial_data/{1,2,3,4}`` static methods to define new data entries, and the ``modify_initial_data/{1,2,3,4}`` static methods to update them once defined) or  from an actor (see the ``class_Actor:define_data/{2,3,4}`` and ``class_Actor:modify_data/{2,3,4}`` counterparts helper functions), thus either initially as well, or in the course of the simulation.

For an actor to be able to use the data-exchange service, it must first update its state with the ``class_Actor:enable_data_exchange/1`` helper function. See ``class_DataExchangeTestActor.erl`` for a complete example. Note then that its subsequent reads (and commits) may happen whether the simulation is already running or not (the latter case applies to initial actors).


So, during any given tick, each actor can freely perform from the data exchanger immediate, direct (zero-latency, as not based on an actor message) read operations of any number of data-sets, and the developer can be confident that this will be done in the most convenient, safe and reliable, performance-efficient, scalable way.

Reciprocally, instead of being read, new data can be defined, or existing data can be modified, provided of course it was declared as non-const (i.e. with the ``mutable`` qualifier), as discussed in the next section.



Modifying Data
--------------

Any already-defined data, whose current qualifier is ``mutable``, can be modified, either initially (in this case the change will be immediate) or in the course of the simulation (in this case the change will occur only starting from the next tick).

This is done thanks to calls to the ``modify_data/{3,4,5}`` helper functions, which on each tick are synchronous operations resulting in the root data exchanger recording all commits that happen during the current tick, and checking them on the fly (ex: no change in `const` data, only up to one commit per data key per tick, etc.). A ``commit`` request is implemented as a direct message sending, from an actor to the root data exchanger (thus not going through the data-exchange tree).

Once this tick is over, should at least one commit have been requested, the root time manager notifies the root data exchanger that we are in-between two ticks and that it can thus propagate safely its commit(s), down the exchanger hierarchy (tree-based propagation).

Once done (i.e. fully acknowledged by a reverse, leaves-to-root, traversal), the root exchanger notifies the root time manager that the next scheduled tick can be triggered.

That way, at the expense of a minimal (and conditional [#]_) inter-tick wall-clock latency [#]_, all subsequent **readings** then done can be performed locally (on an exact local clone of the root exchanger) and instantly (with a zero-tick latency in virtual time): indeed, during a tick, from the point of view of actors, no data can change; their update will happen only in-between ticks, so all reads can be done

.. [#] If no commit is done during a tick, the data-exchanger will incur virtually no overhead, so this data-exchange service will cost resources *only* if used.


.. [#] For synchronisation reasons, the commit propagation has to be part of the critical path of the time manager (the period between two ticks during which no parallel operation can occur). As increasing too much that duration would directly impact the simulation overall performances, we did our best to minimise the inter-tick latency induced by the data-exchange service. For example most checkings are not once a write operation is requested, not later once it is to be actually committed.


Note also that setting a data involves either defining a new key or modifying a value associated to an already-defined key, possibly also updating the qualifier of a key/value pair, provided these operations are licit (ex: a ``mutable`` qualifier may be set to ``const``, not the other way round; a data can be defined only once, and modified any number of times once first defined).


More precisely, data **modification**, during the simulation, is to be done from actors, and is either:

 - defining new data, with ``class_Actor:define_data/{2,3,4}``
 - or modifying pre-existing data, with ``class_Actor:modify_data/{2,3,4}``


The point is that, once the simulation is started:

 - a modification done at tick T will be visible (thanks to read operations) only at the next scheduled tick (ex: T+1)

 - a given data (i.e. a value associated to a key), provided it was declared as ``mutable``, can be modified during the simulation up to once per tick, otherwise the data exchanger will detect the mismatch (commit inconsistency, up to one writer per key and per tick) and crash on purpose the simulation with a relevant exception


So if the data exchanger holds at tick T a ``{my_data,AnyTerm}`` mutable entry, and if during this tick an actor commits a ``{my_data,AnotherTerm}`` entry, then during T all read requests of ``my_data`` will return ``AnyTerm``, and during T+1 they will all return ``AnotherTerm``. If more than one actor attempts to commit at T a change to ``my_data``, then the simulation will fail. Even if they try to commit the exactly the same value.



Practical Use
-------------

The data-exchange service is activated by default, as its runtime overhead in case it is not actually used is fairly low.

It can be explicitly enabled or disabled through the deployment settings, see the ``enable_data_exchanger`` field of the ``deployment_settings`` record, defined in ``class_DeploymentManager.hrl``. It is also the place where the list of configuration files to be used for static information (if any) can be specified.

Data can be defined, then modified and/or read.

When a data is to be read, only its key is to be specified.

When a data entry (key and at least value) is specified either for definition or for modification, if no qualifier is specified, then the default one (``const``) will be implied. This means that requesting a modification with a data entry ``{my_data,AnyTerm``}`` implies notably that:

 - ``my_data`` has already been defined
 - its previous qualifier was ``mutable`` (thus was explicitly set as such), otherwise the modification would be bound to fail
 - its new qualifier will be ``const`` (none was specified in the modification request), thus no further modification will be done on that data

These data operations can be made either from the simulation case (thus, before the simulation is started) and/or from actors (before or during the simulation).



Implementation Details
----------------------

There is one data exchanger agent per computing node, and they form a tree (the exchanger hierarchy), whose root is the root data-exchanger, very much like the one of time managers.

That way commit consistency is verified by the root data exchanger, which holds the sole reference copy of the data repository, which is replicated on all non-root (i.e. local) data exchangers. As updates are made between ticks, during a tick the data is stable, and each actor will be able to read them locally, and at will.

Data definitions and modifications will be recorded during a tick, and applied once that tick is over, before the next scheduled one.

Data can also be defined and modified before the simulation is started. Definitions can be done through requests or through the reading of any number of configuration files.

For actors, data readings are just plain, direct, non-reordered, WOOPER requests that are made locally (on the node of the reading actor), thanks to the local exchanger that was automatically deployed there. Therefore readings should be considered as being by design fast and inexpensive.

From the simulation case, the readings are managed a bit differently internally, depending on whether the user host is included or not in the simulation.

If included, then on that same user host there is a computing node with its own local data-exchanger. As a consequence, to avoid a data duplication of the exchange repository on the user host, the user node will interact with the data-exchanger local to the computing node on the same host.

If the user node is not included in the simulation, then there is not computing host to rely upon, and a local data-exchanger dedicated to the user node is simply created and integrated in the data-exchange hierarchy.


.. include:: Sim-Diasca-spatialised-support.rst
