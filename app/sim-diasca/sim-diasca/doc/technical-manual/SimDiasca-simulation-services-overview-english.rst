:raw-latex:`\pagebreak`


---------------------------------------
A Short Overview of Simulation Services
---------------------------------------

From the point of view of a model writer, the main **functional** simulation services offered by Sim-Diasca are:

 - simulation case definition: see how to choose the simulation settings (``simulation_settings`` record), to specify how the deployment should be performed (``deployment_settings`` record), to define the initial state of the simulation (see the creation API of ``class_Actor``), to parametrise the load-balancing (``load_balancing_settings`` record), to control the simulation (starting time, ending criteria, console tracking, result browsing, etc.)
 - time management: see ``class_TimeManager``, that drives the scheduling of all ``class_Actor`` instances
 - state management, interactions: see ``class_Actor`` and, for specific needs, ``class_BroadcastingActor``
 - stochastic support: see ``class_RandomManager``, ``class_StochasticActor`` and, more importantly, the stochastic API of ``class_Actor``
 - specific data exchanges: see ``class_DataExchanger``
 - result management: see ``class_ResultManager`` and ``class_ResultProducer`` (for the overall organisation), ``class_Probe`` and ``class_DataLogger`` (for the actual result generation) and ``simulation_settings`` record



The main underlying **technical** simulation services are:

 - automatic distributed deployment: see ``class_DeploymentManager`` and ``class_ComputingHostManager``
 - load balancing: see ``class_LoadBalancer``
 - instance tracking: see ``class_InstanceTracker``
 - robustness and reliability: see ``class_ResilienceManager`` and ``class_ResilienceAgent``
 - performance tracking: see ``class_PerformanceTracker``
 - lower-level services: see ``class_Mesh`` for graph support
 - trace management: see the ``Traces`` layer
 - object-oriented support: see the ``WOOPER`` layer
 - distribution, parallelism, process-level scheduling and memory management, etc.: see the ``Erlang`` language
