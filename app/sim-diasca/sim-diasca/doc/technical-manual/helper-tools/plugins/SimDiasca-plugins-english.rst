Plugin Infrastructure
====================

Sim-Diasca offers a full *Plugin Management System*, which allows third-party code to interface with the engine in various ways.



General Principles
------------------

Each plugin is able to:

	- specify requests for configuration changes (ex: like the number of sequencers on each computing node)

	- know runtime information, like the list of the computing nodes that have been actually elected by the engine

	- be notified of all main events (engine-related or even case-defined) regarding the simulation, which for example allows tools to monitor resources during only some phases of the simulation - typically when models are effectively evaluated (skipping deployment, preparation, initialisation phases, as well as the ones after the simulation)



More In-Depth Description
-------------------------

A simulation case can specify a list of directories that will be scanned for plugins at start-up by the engine.

This is done thanks to the ``plugin_directories`` field of the ``deployment_settings`` record, which can be set to a list of directory paths (either absolute or relative to the directory where the case was launched). See ``city_benchmarking_test.erl`` (in ``mock-simulators/city-example/src``) for such an example case.

Each BEAM file found in any of these directories is expected to comply with the interface described in the ``sim_diasca_plugin`` behaviour [#]_ ).

.. [#] This is a post-R15B, Dialyzer-friendly, behaviour. It is defined in ``sim-diasca/src/core/src/plugins/sim_diasca_plugin.erl``.

This boils down to implementing a callback for all simulation events:

 - *on_simulator_start*: when the simulator is started (or almost, as basic services, including the trace one, are already up)
 - *on_deployment_start*: when the deployment phase starts
 - *on_deployment_stop*: when the deployment phase stops
 - *on_technical_settings_available*: when the simulation technical settings are available, notably once the deployment phase is over
 - *on_case_initialisation_start*: when the simulation case starts the creation of the initial state of the simulation
 - *on_case_initialisation_stop*: when the simulation case finished the creation of the initial state of the simulation
 - *on_simulation_start*: when the simulation is started (first tick, first diasca)
 - *on_simulation_bootstrap_start*: when the simulation is just started and must evaluate the first diasca of all initial actors
 - *on_simulation_bootstrap_stop*: when the evaluation of the first diasca of all initial actors is over
 - *on_simulation_wallclock_milestone_met*: when a wallclock milestone is met (i.e. when a given duration in real time elapsed)
 - *on_simulation_tick_milestone_met*: when a tick milestone is met (i.e. when a given number of ticks have been evaluated)
 - *on_simulation_stop*: when the simulation is stopped (an ending criterion was just met; only called on successful ending)
 - *on_result_gathering_start*: when the results start being gathered, after simulation termination
 - *on_result_gathering_stop*: when the results have been gathered
 - *on_simulator_stop*: when the simulator execution stopped under normal circumstances (i.e. not crashing)
 - *on_case_specific_event*: triggered iff a case decided to notify the plugins of a specific event


Most callbacks just take one parameters, the current plugin state, and return one value, the new plugin state.

This allows stateful plugins to be easily implemented (instead of, for example, spawning a dedicated and registering it), the engine keeping track of their state between callbacks. Of course using that feature is optional and, for example, all callbacks can ignore the engine-specified state and only return the ``undefined`` atom as new state.

Some callbacks are a little more complex:

 - ``on_simulator_start/2`` is given also the current configuration changes (as possibly already updated by other plugins - the first of them receiving a blank ``configuration_changes`` record), so that the currently executed plugin can update it

 - ``on_technical_settings_available/2`` is given also a ``technical_settings/2`` record, describing the actual settings then enforced by the engine, notably on the basis of the previously consolidated plugin configuration requests

 - ``on_simulation_wallclock_milestone_met/2`` and ``on_simulation_tick_milestone_met`` are respectively also given the current simulation timestamp (respectively in real and virtual time), whenever a milestone is met

 - ``on_case_specific_event/3`` is given the name of the corresponding case-specific event (as an atom) and its associated data; see ``class_CityGenerator.erl`` for an example of use






Implementation Notes
--------------------

All services of the lower layers are available to plugins. As a result, they can rely upon the facilities of the Common layer, WOOPER classes may be used as plugins, and the distributed trace system can be used by the plugins as well.

The plugin service is mostly located in ``sim-diasca/src/core/src/plugins``.

An example of a full, complete plugin is available in ``plugins/tests/my_plugin_example.erl``.

We might consider supporting even *distributed* plugins (with one instance of them, code and data, being deployed and run on each computing node), if such use case was found interesting.
