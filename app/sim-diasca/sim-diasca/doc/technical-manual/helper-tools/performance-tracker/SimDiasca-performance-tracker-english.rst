Performance Tracker
===================


The *Performance Tracker* is a fully optional mechanism that allows the user to monitor a simulation that was run, in order to profile it.

It aims to monitor and then generate reports giving detailed information about all kinds of technical measurements dealing with performances, notably in terms of resource consumption (CPU, RAM, network, Erlang process count, etc.).



Requirements & Functional Coverage
----------------------------------

The current version provides the following features:

 - trace the following memory consumptions on the user node and on computing nodes along with the simulation duration or with the wall-clock time:

   - the available memory (which is: free memory + buffers + cache)

   - the memory used by the other applications (i.e. all non-Erlang applications)

   - the memory allocated to the Erlang emulator (i.e. the memory currently used by the simulator), plus any other Erlang program being executed at the same time

   - the used swap

 - trace the overall number of Erlang processes (on a per-node basis) during the simulation in real (wall-clock) time and/or in simulation time (tick)


In the future, it will allow to trace also:

 - overall number of instances of a given class; for example: how many classical or virtual probes, or ``class_Foobar`` instances on each node

 - the wall-clock duration of each simulation tick, on each node

The output of the performance tracker is:

 - time series (``.dat`` files)
 - and/or the corresponding graphs (``.png``)

Its reports are available post-mortem (not live), among the simulation results.



Implementation
--------------

The performance tracker and its integration test are located in ``sim-diasca/src/core/src/data-management/monitoring/performance``.

The performance tracker is integrated at the Sim-Diasca level (i.e. not only at the WOOPER-level), and relies on various services of the engine (ex: probe, deployment manager, result manager).


The performance tracker is implemented as an optional service, disabled by default (to avoid any unnecessary runtime overhead), which can be enabled on a per-need basis.

It defines following classical probes:

 - a global probe is created in order to track the overall number of Erlang processes on each node: it includes as many curves as there are nodes (user node and computing nodes); each curve is named according to its corresponding node

 - a probe per node is also created in order to track the resource consumptions on each node



In terms of architecture, currently the performance tracker is centralized (a singleton) and is created on the same node with the Depployment Manager in order to enable the performance information available in all circumstances, such as, simulation fail because of a timeout or a computing node crash.

In case of performance tracker enabled, the data for the probes is retrieved in a predefined interval during simulation, by default, this interval is defined as 100ms and it can be modified by sending a "setTickerPeriod" message with new interval to the performance tracker.

In the future and in particular for the scalability test when numerous computing nodes are engaged, there will be a performance monitor per node besides this centralized tracker, in order to reduce the exchanges over the network. Contrariwise, the distributed monitoring way can lead to lose some node performance information when a node is crashed during the simulation and relevant data saved on it.



Performance Tracker Activation and Creation
-------------------------------------------

The activation of the performance tracker is to be requested from the simulation case, through its deployment settings: the ``enable_performance_tracker`` field of the ``DeploymentSettings`` record (defined in ``class_DeploymentManager.hrl``) must be set to true.

When the performance tracker is enabled, the user can customise it according to her needs, notably in order to set the wall-clock period, expressed in milliseconds, between two measures (performance tracker samples); see its ``setTickerPeriod/2`` method.

To do so, the PID of the tracker must be obtained, thanks to the following static method::

  MyPerformanceTrackerPid = class_PerformanceTracker:get_tracker()



Performance Tracker Report Example
----------------------------------

 - Example 1: a simulation is lanced on one node:

 These reports are generated based on the results of the performance tracker test (``class_Performance_Tracker_test.erl``) with following test configuration:

   - only one initial test actor (``class_PerformanceTracker_TestActor``)

   - each test actor creates a new test actor every 4 simulation tick

   - the total simulation duration is 36 simulation ticks

   - the simulation runs on only one machine, that means: the user node and the computing node are on the same machine

:raw-latex:`\pagebreak`

One can have a global view of the simulation, first in *wall-clock* time:

:raw-html:`<img src="example1_process_tracker_in_wall_clock_time.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example1_process_tracker_in_wall_clock_time.png}`


The same global view can also be shown in *simulation* time:

:raw-html:`<img src="example1_process_tracker_on_simulation_time.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example1_process_tracker_on_simulation_time.png}`



More detailed information can be collected, on a per node basis. Here first is the report specific to the *user* node, in wall-clock time, then the same report for a *computing* node:

:raw-html:`<img src="example1_memory_tracker_on_user_node.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example1_memory_tracker_on_user_node.png}`


:raw-html:`<img src="example1_memory_tracker_on_computing_node.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example1_memory_tracker_on_computing_node.png}`


 - Example 2: a simulation runs on 4 nodes:

 These reports are generated based on the results of the performance tracker test (``class_Performance_Tracker_test.erl``) with following test configuration:

 - only one initial test actor (``class_PerformanceTracker_TestActor``)

 - each test actor creates a new test actor every 4 simulation tick

 - the total simulation duration is 36 simulation ticks

 - the simulation runs on 4 machines, that means:  the user node and one computing node are on one machine, the three other computing nodes are on three different machines

:raw-latex:`\pagebreak`

One can have a global view of the simulation, first in *wall-clock* time:

:raw-html:`<img src="example_process_tracker_in_wall_clock_time.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example_process_tracker_in_wall_clock_time.png}`


The same global view can also be shown in *simulation* time:

:raw-html:`<img src="example_process_tracker_on_simulation_time.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example_process_tracker_on_simulation_time.png}`


More detailed memory consumption information can be collected. Here first is the report specific to the *user* node, in wall-clock time, then the same report for  *computing* nodes:

:raw-html:`<img src="example_memory_tracker_on_user_node.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example_memory_tracker_on_user_node.png}`


:raw-html:`<img src="example_memory_tracker_on_computing_node_1.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{example_memory_tracker_on_computing_node_1.png}`


.. comment Not useful enough:
 :raw-html:`<img src="example_memory_tracker_on_computing_node_2.png"></img>`
 :raw-latex:`\includegraphics[scale=0.2]{example_memory_tracker_on_computing_node_2.png}`

 :raw-html:`<img src="example_memory_tracker_on_computing_node_3.png"></img>`
 :raw-latex:`\includegraphics[scale=0.2]{example_memory_tracker_on_computing_node_3.png}`

 :raw-html:`<img src="example_memory_tracker_on_computing_node_4.png"></img>`
 :raw-latex:`\includegraphics[scale=0.2]{example_memory_tracker_on_computing_node_4.png}`
