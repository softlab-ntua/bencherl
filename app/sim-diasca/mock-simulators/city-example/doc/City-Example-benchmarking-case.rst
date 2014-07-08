========================================================================
Some Information About The Sim-Diasca **City-Example** Benchmarking Case
========================================================================


+------------------------------------------+--------------------------------------+
| .. image:: sim-diasca.png                                                       |
|   :scale: 40                                                                    |
|   :align: center                                                                |
+==========================================+======================================+
| .. image:: logo-EDF-english.png          | .. image:: lgpl-v3-logo-bordered.png |
|   :scale: 50                             |   :align: right                      |
|   :align: left                           |                                      |
+------------------------------------------+--------------------------------------+


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Organisation: Copyright (C) 2012-2014 EDF R&D
:Author: Olivier Boudeville
:Contact: olivier.boudeville@edf.fr
:Creation Date: Thursday, March 14, 2013
:Lastly Updated on: Thursday, March 20, 2014
:Status: Work in progress
:Version: 0.1
:Dedication:

	For people interested in this `Sim-Diasca` urban benchmarking case, wanting to run and/or parametrise it.
:Abstract:


This simulation case has been written in order to provide a open, sharable, tractable yet representative use case of Sim-Diasca, notably for benchmarking purposes.

As a consequence, it is designed to be potentially largely scalable, both in terms of duration and size (i.e. it *can* be launched for almost arbitrarily large simulations), yet exhibiting the typical issues that most real-life cases exhibit (ex: sequential phases becoming acute problems, new bottlenecks appearing as the scale increases, different timings and nature of resource consumption, etc.).


.. meta::
   :keywords: Sim-Diasca, massive, simulation, multi-agent, development, benchmarking, urban, city



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 2

.. section-numbering::





:raw-latex:`\pagebreak`





Understanding the simulation case
=================================

This specific simulation attempts to represent a rather useful part of a city - the one that deals with waste management.

Basically, a fake, modelled, city has to be procedurally generated first. For that a number of waste sources (residential or industrial), incinerators and landfills are defined, and a road network (made of roads and road junctions) is generated to interconnect them.

A pool of waste trucks then strives to transport wastes (multiple kinds of them are defined) so that the garbage produced by the various waste sources is collected, transformed into incinerators, resulting in bottom ash that is then to be transported further, in landfills.

Hopefully none of the waste storage facilities  will be saturated in the process, incinerators will be appropriately fed, and waste will not accumulate in the chain.


Such a city, modelled regarding wastes, includes following elements:

 - *waste sources*, which are either residential (numerous, each producing small quantities of various waste types) or industrial (a few of them, producing large quantities of other waste types)

 - *incinerators*, each being able to burn some of these types of waste (the duration of this process depending on several factors, including which tank is used, the kind of waste and the burners that are available for that), but will in turn produce non-incinerable waste (bottom ashes)

 - *landfills*, which are able to store *all* kinds of wastes (incinerable or not), but are not able to transform them

 - *waste trucks*, which are able to transfer wastes from a point to another, based on their logic (state machines with a queue of intents and some opportunistic traits),limited storage and possibilities of mixing wastes, and limited knowledge of their surroundings [#]_

 - a *road network*, which allows vehicles (currently, only waste trucks) to reach points of interest; this is a directed, cyclic graph whose nodes are the previous elements (ex: an incinerator or a road junction) and whose edges are roads (with lengths and capacities, their load affecting the speed of vehicles on them); this network is represented twice: first as a dedicated graph (in an associated global ``RoadNetwork`` instance, in our little ``GIS`` - currently not used in the course of the simulation) and secondly as the superposition of the information present in each point of interest and road (at this level, the information is even duplicated, as roads and points of interest both know their local connectivity)


.. [#] These disaggregated, individual-based simulations rely only upon decentralised, partial information; for example no agent - except, before the simulation starts, the mini-GIS - has a total knowledge of the road network (which does not exist as such, it is merely an implicit graph).


A small instance of generated road network is (zoom-in for a better view):

:raw-html:`<img src="Road_Network_2D_Topology-example.png"></img>`
:raw-latex:`\includegraphics[scale=0.15]{Road_Network_2D_Topology-example.png}`


The impact of the load of a road, whose speed limit is 110 km per hour in the example below, onto the average speed of vehicles on it is based on this model, ruled by the number and size of vehicles using it:

:raw-html:`<img src="road-characteristics.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{road-characteristics.png}`


Finally, the main classes and models of interest are:

:raw-html:`<img src="city_example_class_diagram.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{city_example_class_diagram.png}`


:raw-latex:`\pagebreak`



Running the simulation case
===========================


Quick procedure
---------------

The simplest approach is, once Sim-Diasca is built (please refer to the *Sim-Diasca Technical Manual* for that), to:

 - go to the City-Example directory: ``cd mock-simulators/city-example/src/``

 - then run: ``make batch``



This will run this case with default settings (duration: short; scale: tiny), in batch mode (no trace monitoring, no display of graphical information or of results - hence minimising the prerequisites needed).

Only the output of the console tracker (i.e. the base information summarised in an array printed on the user console) will be shown.

For this very specific case, the simulation termination will not halt the VM, as, for benchmarking purposes, one might want to execute a series of simulations from the same Erlang shell.


More Details
------------

First, Sim-Diasca must be properly installed. Please refer to the *Sim-Diasca Technical Manual* for that. Most prerequisites (notably the trace browser) can be disabled here.

The code of City-Example is located in the archive in ``mock-simulators/city-example/src``.

A model of a city is generated by the City-Example simulation case (see ``city_benchmarking_test.erl``).

The user may tune the size of the simulation (either from the command-line or directly from the simulation case) in terms of space (city size) and time (frequency and duration of the simulation).

For example::

  make city_benchmarking_run CMD_LINE_OPT="--batch \
	   --duration short --scale tiny"


The city generator (see ``class_CityGenerator.erl``) will determine, based on the city description (see the ``city_description`` record), the various elements that compose this city (procedural generation).

Some rules for that generation are applied; for example each point of interest must have at least one inbound and one outbound road (full-connectivity, and no traffic source or sink); however the generated road network is not necessarily fully-connected, in the sense that there could be "islands" that may not be connected with the rest of the network.

The city generator will create randomly the specified numbers of these various elements, ensuring that some constraints are respected (ex: a minimum distance between two elements of the same type, another minimum distance between two elements of any type).


The aforementioned ``make batch`` command simply corresponds to::

  make city_benchmarking_run CMD_LINE_OPT="--batch \
	--duration short --scale tiny"                 \
	EXECUTION_TARGET=production

(not that ``EXECUTION_TARGET`` is a compile-time option, so it would apply here only for any module that would have been to be rebuilt)


To disable the batch mode, you can remove the ``--batch`` option below or simply run ``make trace`` instead. Supposing the relevant prerequisites have been installed beforehand, you should then be able to monitor (live and/or post-mortem) any enabled traces (depending on whether the code has been compiled with ``EXECUTION_TARGET=production``), the generated road network, and the graphs over time of the production, transport, treatment and storage of the various types of wastes supported.

Both duration and scale settings can be overridden from the command-line; for example: ``make batch CASE_DURATION=long CASE_SCALE=huge``.

As stated in ``city_benchmarking_test.erl``:

  - benchmarking_duration() :: 'brief' | 'short' | 'medium' | 'long'.
  - benchmarking_scale() :: 'tiny' | 'small' | 'medium' | 'large' | 'huge'.


If these settings were too coarse, this file is pretty straightforward to modify to further tune duration and/or scale, or even to decide how the simulated city should be (notably in terms of area [#]_ and of number of points of interest of each type).

.. [#] Increasing the number of points of interest may trigger ``location_generation_failed`` exceptions, as a minimum distance between these points (which depends on their type) is enforced, while the declared city area may not be sufficient for that. The solution is simply to increase in turn that declared area.


As soon as larger cases are run, one should switch from the default execution mode (which is ``development``, to ease the troubleshooting) to the ``production`` mode. Then a number of technical measures will be applied, including: relaxed time-outs, removal of most trace sending, disabling of optional checkings, etc.

To perform that mode switch, one has to rebuild Sim-Diasca; from its root::

  $ make clean all EXECUTION_TARGET=production

Otherwise the trace aggregation would most probably hog most of the resources.

By default this case will be run only locally (on the user host). Going distributed only involves creating, in the current directory, a text file named::

  sim-diasca-host-candidates-for-scale-benchmarks.txt

listing the networked candidate hosts that are allowed to take part to a simulation.

A simple configuration file could then be::

  sonata.
  wanderlust.
  'red.foo.org'.
  eturanis.

(assuming that these are hosts whose DNS names can be resolved from the user host, and that a password-less SSH connection to them is possible). For the complete syntax and examples, see::

  sim-diasca/conf/sim-diasca-host-candidates-sample.txt


Currently, exactly one Erlang node is created per host, federating all local cores of all local processors.

No software prerequisites are assumed on these hosts, except that a compatible version of Erlang must be available from their connection default PATH: Sim-Diasca will take care of the full deployment of code and data, including the selection, sending and registration of BEAM files on each host, in parallel.

Very clear console messages will be output to know precisely which of the candidate hosts could be selected.

Various other technical settings can be changed by editing ``city_benchmarking_test.erl``, including settings dealing with simulation (see the ``simulation_settings`` record) and with deployment (see the ``deployment_settings`` record).

Finally, to run cases with increasing durations and scales, one may execute the ``run-full-test.sh`` script.



Some notes about the case
=========================


Quality
-------

The case was tested with R16B; the build is clean (no compiler warning issued). Dialyzer has been run against its full code base (all layers) and no problematic report was then identified.



Stability
---------

Most of the initial state of the simulation (ex: the full road network, the state of the various points of interest) is procedurally generated, in order to avoid having to create input samples, and, more importantly,to be able to scale it at any level.

An effort has been made in order to decrease the risk of generating a case that would trigger simulation-time violations of assumptions (ex: an industrial waste source could be created in a non-connected part of the road network that no waste truck could collect, resulting in the runtime detection of an overloaded waste tank; as a consequence, now these tanks will stay at their maximum load level instead).

However some hard-to-predict faulty generated cases could linger. For example, a too fast vehicle on a too short road could result in shorter transit durations which, once quantised as an integer number of simulation ticks, could, depending on the simulation frequency, lead to a relative error that could exceed the case-specific allowed threshold (however our default threshold has been relaxed, so it should not happen). Some rare floating-point numerical errors have been taken care of.


Parallelism
-----------

The main part of the simulation (the evaluation of model instances over simulation time) is intensely parallel; for a correctly-loaded *local* simulation, on average each core is loaded at more than 75% (example for a simulation run locally on a 8-core laptop); for *networked* simulations, the load may considerably vary and may be significantly lower, because of the latency that the network induces.

Of course, if a simulation is too small compared to the available resources, their usage will remain low.

However, with a simulation that is too large for its dedicated resources, performances will drop *very* significantly [#]_.

.. [#] This phenomenon would deserve some analysis, as then the overall computing power will be quite small compared to a normal load, whereas it should stay roughly the same (the simulations should in theory just last proportionally longer).


So the best approach is to determine the sweet spot and to ensure that the computing resources are sized according to the scale of the simulation.

Before the simulation starts, a few less parallel actions take place (ex: generating procedurally the initial situation, creating the inter-linked actors, etc.).

One of them hits very hard on the (simplistic) GIS this case embeds, which is a typical bottleneck whose cost is quickly rising with the scale of the simulation, becoming prohibitive as soon as we exceed the ``medium`` setting.

Of course we could parallelise this step as well (by going for example for space-based partitioning, like `BSP <http://en.wikipedia.org/wiki/Binary_space_partitioning>`_), but, at least currently, we see it as a part of the problem that is interesting as such: if we submitted only perfectly scalable cases, not only there would be little point in these scalability studies, but these benchmarking cases would not be representative of the actual simulations that are performed.


For a case that is either CPU-bound or network-bound, the more insufficient resources are, the more frequently the simulation stalls will happen, until stopping due to exceeded time-outs. For a case that is too much memory-bound, at least one host will hit the swap and slow down considerably the simulation (to the point that stopping it is often the best option).

So a very good loading should result on an average load exceeding 75%, while no simulation stall is detected by the watchdog.

Note that a message notifying of a simulation stall, like::

 Simulation currently stalled at tick offset #XXXX (being at
 diasca XX) for time manager <XX.XX.0>, still waiting for a
 total of X notification(s) of end of diasca
 [...]

is not the sign of an error, but only a warning telling that the simulation scheduler determined that the pace of diasca evaluations is considerably slower than usually seen. Therefore this is merely the sign that the simulation could execute better.

There are additional mechanisms to detect real, actual problems (ex: non-terminating actor, deadlock in the system, etc.) and also to stop the simulation should it become unbearably slow.



Expected Behaviour
------------------

The various phases of the simulator execution should show very different behaviours when the scale increases.


Overall, we expect this kind of outcome:

 .. image:: typical-scalability-edited.png
   :scale: 80
   :align: center




Benchmarking Hints
------------------

This case is now a lot more complex than initially, with some more involved logic (ex: the waste trucks, the incinerators), many rich exchange patterns and some complex queries (ex: breadth-first pathfinding to establish loading/unloading routes with varying predicates).

However it is still be expected to be deemed "talkative", communication-bound, as most the aforementioned actions, notably interactions between model instances, are implemented thanks to Erlang messages that, for synchronisation purposes (more generally, to preserve the simulation properties), are exchanged between these model instances, time managers, probes, etc.

So, maybe this example is not CPU-bound, but it corresponds to a reality we often experience with that tool.

Note that benchmarking distributed applications will surely raise issues with deployment, as each of such applications is expected to connect to other hosts, launch their own VMs, send them their relevant pieces of code and data, etc.

For example, in Sim-Diasca, no distinction is made between purely local or distributed simulations: the former is simply a special case of the latter, where only one host is involved. For that reason, in all cases, in addition to the *user* node (i.e. the one run from the command-line, when executing ``make foo_run``) which drives the simulation, the actual model evaluation is done in *computing* nodes (there is at least one of them), that are created based on the deployment settings.

So benchmarking only the user node has little interest, as most of the actions will take place in these computing nodes (even in the local case, where, as usual, a computing node will be created, alongside the user node, on that single host).

Only one shell script (``common/src/scripts/launch-erl.sh``) is needed to run a Sim-Diasca case (internally used by the make system).

If necessary, this can be replaced by running directly from the command-line the final command it executes. See the ``launch-shell-for-sim-diasca.sh`` script for that.


Another (less attractive) option to gather simply all the information (code mostly, as this case does not require the deployment of data) is to request Sim-Diasca not to discard the archive it creates locally as part of its deployment process.

This is a ``*.sdar`` file - for *Sim-Diasca Archive* - which is a ZIP file which contains all BEAMs and data files that are needed by the distributed computing nodes (since no prior install is requested before launching a simulation).

Note though that some pioneer modules are sent over the network to manage that archive, and thus probably cannot be found in it. So the previous script remains most probably the best solution if wanting to run a simulation directly from the shell.




Planned Enhancements
====================

For this simulation case, there is surely plenty of room for improvement in most fields: the GIS could be parallelised, the simulation case could give hints for a far better instance placement, the engine by itself could harness the computing resources a lot better, etc.


Following model-level changes could be considered:

 - the trucks should not always select the closest target POI, as returned by the breadth-first search; otherwise they tend to adopt a too regular behaviour

 - the Waste Operating Center should be integrated: POI having overloaded waste tanks would call it, and it would then in turn spot the nearest/less busy truck and request it to unload as soon as possible (if not empty) and then to go unloading the calling POI, on a priority route

 - the road network should be checked for connectivity: if there are parts of it that make islands, additional roads should be created to ensure that only one network exists
