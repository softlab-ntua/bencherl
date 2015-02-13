:raw-latex:`\pagebreak`


Sim-Diasca Conventions
======================

In this section the conventions to be respected with regard to Sim-Diasca are detailed.



Thou Shalt Not Bypass The Simulation Engine
-------------------------------------------

The Sim-Diasca services should be used whenever applicable. Notably, bypassing the simulation mechanisms (for actor creation, communication, deletion, etc.) is absolutely prohibited, as essential properties, like the respect of causality, would then be lost.

Such a short-time "simplification" would be considered still more harmful than others like the use of the ``goto`` statement in ``C``:

:raw-html:`<img src="xkcd-goto.png"></img>`
:raw-latex:`\includegraphics[scale=5.5]{xkcd-goto.png}`




Proper Inter-Actor Communication
................................

To implement the communication between two actors, neither plain Erlang messages nor arbitrary WOOPER messages can be used: as we aim to develop a distributed *simulation*, as opposed to a mere distributed *application*, we have to make use of the Sim-Diasca mechanisms for automatic inter-actor message reordering.

This means that, for inter-actor communication purpose, *only* the following helper functions shall be used:

 - for the vast majority of cases: ``class_Actor:send_actor_message/3``

 - for the very special case where the same message is to be sent to a *very large* number of other actors (typically dozens of thousands), from an instance of ``class_BroadcastingActor``: one should use ``send_actor_message/3``, ``send_actor_messages/3`` or ``send_actor_messages_over_diascas/3``, as defined in the latter class


Of course one may rely on higher-level specialization layers, making use of Sim-Diasca but providing another API. The point is that inter-actor messages should be ultimately managed by Sim-Diasca, rather than being sent directly with the ``!`` operator provided by Erlang.



Proper Actor Life-Cycle
.......................

Very similarly, life-cycle of actors should be ultimately managed by the engine, not directly by the developer: the creation and termination of actors must respect the conventions detailed below.



Proper Actor Creation
_____________________

An actor can be created either initially (before the simulation is started) or at simulation-time (i.e. in the course of the simulation), as detailed in next sections.

Calling directly (i.e. from the user code) any ``spawn`` variation or any WOOPER ``new`` variation (ex: ``remote_new_link``) is totally prohibited: we must rely on the Sim-Diasca mechanisms, and not attempt to bypass them.

The actual creation will be performed by the load balancer, on a computing node of its choice, and the placement will be fully transparent for the model writer.

Whether an actor is initial or not, construction parameters are to be supplied for its creation.


These construction parameters will be specified as a list, for example ``[_Age=5,_Gender=male]``. The matching constructor *must* then be in the form of::

  construct( State, ActorSettings, P1, P2 ) ->


Note that two additional initial parameters appear here:

 - ``State`` (whose type is ``wooper:state()``), which corresponds to the engine-supplied blank initial state this instance starts with

 - ``ActorSettings`` (whose type is ``class_Actor:actor_settings()``), which will be provided automatically by the load balancer too, at runtime; this parameter is just to be listed by the model developer in the constructor of the model, when calling its parent constructor which inherits, directly or not, from ``class_Actor``, as shown below.


So a typical constructor for a model ``class_M1``, inheriting from, for example, ``class_SpecialisedActor`` and from classes ``class_C1`` and ``class_C2``, could be::

  % Constructs a new instance of class_M1:
  construct( State, ActorSettings, P1, P2, ... ) ->
	 % Will result ultimately in a call to
	 % class_Actor:construct( State, ActorSettings, AName ):
	 SpecialisedState = class_SpecialisedActor:construct( State,
		ActorSettings, ... ),
	 C1State = class_C1:construct( SpecialisedState, P1, ... ),
	 C2State = class_C2:construct( C1State, P1, P2, ... ),
	 [...]
	 FinalState.

Note how states are chained (one being built on top of the other), from the blank, initial one (``State``) to the one corresponding to the complete initial state of the instance (``FinalState``), as returned by the constructor.


We encourage the use of type specifications, which would be here::

 -spec construct( wooper:state(), class_Actor:actor_settings(),
	  type_of_p1(), type_of_p2() ) -> wooper:state().







Initial Actor Creation
**********************


Basics
::::::

An abstraction API is available to create from a simulation case *initial* actors, i.e. bootstrapping actors, which are created before the simulation is started.

It is generally based on the ``class_Actor:create_initial_actor/2`` static method::

  ActorPid = class_Actor:create_initial_actor( ActorClassName,
	ActorConstructionParameters )

For example, in ``my_example_test.erl`` we could have::

  ActorPid = class_Actor:create_initial_actor( class_PinkFlamingo,
	[ _Age=5, _Gender=male ] )


Should multiple initial actors have to be created, using this method would be less than optimal, as the load-balancer would be looked-up in the process registry at each call of this static method, which, if creating thousands of actors in a row, could induce some overhead.

Therefore a more efficient alternative is available, the  ``class_Actor:create_initial_actor/3`` static method, for which the PID of the load-balancer is to be specified as a parameter, having thus to be looked-up only once in the simulation case::

	LoadBalancerPid = LoadBalancer:get_balancer(),
	FirstActorPid =  class_Actor:create_initial_actor( Class1, Parameters1,
	  LoadBalancerPid),
	SecondActorPid = class_Actor:create_initial_actor( Class2, Parameters2,
	  LoadBalancerPid),
	[...]



Multiple Parallel Creations
:::::::::::::::::::::::::::

A typical use case is to load from any source (file, database, etc.) a set of construction parameters for a large number of instances.

For larger cases, creating actors sequentially may lead to very significant simulation start-up durations.

In such cases, ``class_Actor:create_initial_actor/1`` should be used instead : then a smart, parallel, batched creation will be done, allowing to create all instances as efficiently as reasonably possible.

This results in a considerably faster creation of the initial state of the simulation, provided there is no dependency between the created actors in the specified batch. Otherwise actors should be created in multiple stages, to ensure that the PID of the prerequisite actors is already known and can be specified at a later stage, when in turn creating the actors whose constructor requires these PIDs.

For non-programmatic, file-based initialisation, we strongly recommend using our rather advanced loading system, as described in the technical guide (see its ``Sim-Diasca Management of Simulation Inputs`` section).


Synchronicity
:::::::::::::

All initial operations (i.e. all operations to be triggered before the simulation starts) must be synchronous, to ensure they are indeed finished once the simulation is run: the simulation case has to wait for their completion before greenlighting the start the simulation.

This involves the use of:

 - synchronous creations, which is already enforced by the aforementioned ``class_Actor:create_initial_actor{2,3}``, etc. static methods

 - requests rather than oneways, once instances are created and the simulation case intends to act upon them (for example in order to link them together); requests must be used, not necessarily in order to retrieve a potential result, but at least to ensure that they are fully processed before the simulation starts (hence the need of using a receive; from the simulation case, one shall prefer using ``test_receive/0`` or ``app_receive/0`` - both exported by the ``Traces`` layer - rather than classical ``receive`` constructs, see below)


Otherwise there could be a race condition between the end of these initial operations (which may take any time) and the triggering of the simulation start (a message which, without flow control, could be sent too early by the simulation case).



Nested Creations
::::::::::::::::

When creating initial actors, we might find useful to create an actor A that would create in turn other initial actors, and so on (nested creations).

This is possible, however these creations should not be directly done from the constructor of A, as this would lead to a systematic deadlock by design [#]_. Some solutions have been identified, but they were not satisfactory enough [#]_.

.. [#] A deadlock will occur because the load balancer will be blocked waiting for the creation of actor A to finish, thus paying no attention to the requested creations in-between, while they themselves are waited for the creation of A to complete.

.. [#] A non-blocking solution could be to have a load balancer that does not wait for an instance to acknowledge that its spawn is over: the load balancer would thus return immediately and keep track of the ``spawn_successful`` message (interpreted as a oneway) that it should receive before the simulation starts.

  However in that case no total order in actor creation seems to be possibly guaranteed: actor A could create B and C, which themselves could, after some processing, create others actors. As a consequence B and C would create them concurrently, and, depending on various contextual factors, their creation requests could be received by the load balancer in no particular order, leading to a given actor bearing different AAI from one run to another. Nested creations would thus be obtained at the expense of reproducibility, which is not wanted.


Instead, the constructor of A should just create A and return, and the actual creations of other actors should be triggered by a subsequent method call (a request, not a oneway, as explained in the Synchronicity_ section).

For examples, in ``my_creation_test.erl``, we could have::

  [...]
  ActorAPid = class_Actor:create_initial_actor( ClassA,
	ParametersForA ),
  ActorAPid ! { createDependingActors, [], self() },
  actors_created = test_receive(),
  [...]


Note that ``test_receive/0`` corresponds to a safer form than ``receive { wooper_result, R } -> R end``. It is logically equivalent, but immune to interfering messages that could be sent to the simulation case by other Sim-Diasca services (ex: notifications from the trace supervisor).




Simulation-time Actor Creation
******************************

Once the simulation is started, an actor can *only* be created by another one (for example it then cannot be created directly by the simulation case itself), so that a correct simulation time can be enforced.

The creating actor should call the ``class_Actor:create_actor/3`` helper function for that creation, like in::

  CreationState = class_Actor:create_actor( Classname,
	ConstructionParameters, State ),
  [...]


If called at simulation timestamp ``{ T, D }``, then the specified actor will be actually created (by the load-balancer) at ``{ T, D + 1 }``, and at ``{ T, D + 2 }`` the creating actor will know (as its ``onActorCreated/5`` method will be called) the PID of the just created actor.

The creating actor - and any other actor that will be given the returned PID - can then freely interact with the created actor (of course thanks to actor messages), exactly as with any other actor (once its creation is performed, there is no difference between an actor created in the course of the simulation and an initial actor).



Creation With Placement Hints
*****************************

Regardless of whether a creation is to happen initially or on the course of the simulation, it is often a lot more efficient to ensure that sets of actors known to be tightly coupled are created on the same computing host (i.e. are co-allocated).

Otherwise these actors would be scattered by the load balancer on multiple computing hosts according to its placement policy, i.e. regardless of their relationship (since the load balancer has no a priori knowledge about the interactions between models), which would lead in the general case to a useless massive network overhead, and thus to simulations that would be considerably slowed down.

Sim-Diasca offers a way of forcing co-allocation (i.e. to ensure that a set of actors will be in all cases created on the same computing host, no matter of which host it is), thanks to *placement hints*.

A placement hint can be any Erlang term (atoms are generally used for that purpose), that can be specified whenever an actor is created. The engine guarantees that two actors created with the same placement hint will end up being instantiated (by the load balancer) on the same computing host [#]_.

.. [#] Unless a compute node was lost in the course of a simulation that recovered from it.

So Sim-Diasca provides a counterpart to its basic creation API, whose functions are just expecting one extra parameter, the aforementioned placement hint:

 - ``class_Actor:create_initial_actor/{2,3}`` have ``class_Actor:create_initial_placed_actor/{3,4}`` counterparts

 - ``class_Actor:create_actor/3`` has a ``class_Actor:create_placed_actor/4`` counterpart

Except the hint specification, these functions work exactly as their counterpart (ex: w.r.t. the call to ``onActorCreated/5``).

For example, if devices in a house were to be modelled, and if a large number of houses was to be simulated, then for house 437, the placement hint (as an atom) ``house_437`` could be specified for the house creation, as well for the creation of each of the devices it will contain.

That way they would be all created and evaluated on the same computing host, exchanging numerous local messages with no need for costly and slow networked messages.







:raw-latex:`\pagebreak`

Proper Actor Termination
________________________


Removing an actor from the simulation is a bit more complex than inserting a new one, due to pending inter-actor relationships that may interfere with the actor termination.


An actor A should not decide that another actor B is to be removed immediately from the simulation. Notably, sending a ``delete`` message to B means just calling directly the WOOPER destructor and therefore bypassing the Sim-Diasca simulation layer and making the simulation freeze or fail on error [#]_.

.. [#] Indeed actor B would then terminate immediately, either causing the time manager to wait for it unsuccessfully (if the tick of B was not finished yet) or possibly making it be removed from the simulation whereas another actor could still send an actor message to it, thus being blocked forever, waiting for an acknowledgment that would never come. Moreover the time manager intercepts actor deletions and checks that they were indeed expected.

Instead the actor A should send an actor message to actor B (if ever B is not just to terminate solely on its own purpose), resulting on the corresponding oneway of B to be triggered. Then B may or may not choose to terminate, immediately or not. Alternatively B may, by itself, determine it is time for it to be removed from the simulation.

In any case, B will decide that it terminates, at ``{ T, D }``. The main conditions for its deletion is that:

 - there is no more spontaneous action that is planned for it: actor B should not plan anymore a future action, and it should withdraw from its time manager any already-planned future action(s); on termination this will be checked by the time manager, which would then trigger a fatal error if at least one spontaneous action was found for the terminating actor

 - no other actor will ever try to interact with it (i.e. with B) once it will have terminated; for that, usually B has to notify other actors of its termination, so that they can "forget" it (to ensure that they will never attempt to interact with B again); it is up to the corresponding models to ensure of such an agreement, based on the deferred termination allowed by the API detailed below


To emphasize more, the model developer should ensure that, once an actor is terminated, no other actor expects to interact with it anymore (i.e. that all other actors should stop sending actor messages to it). The objective is therefore to delay appropriately the triggering of the termination of an actor until all possibilities of outside interactions are extinguished.


The smallest duration for a termination procedure cannot be automatically determined, as the PID of the terminating actor (B) can have been transmitted in the meantime from actors to actors. Therefore it is the duty of the developer to ensure that a terminating actor B is safely unregistered from all the actors that may interact with it in the future (generally a small subset of the ones that know its PID). Often this unregistering procedure is best done directly from the actor B itself. Then only B can safely terminate.


Two options exist for a proper termination procedure:

 - either to simply postpone the deletion of B until the end of the current *tick* (``T``), letting all diascas that are needed in-between elapse, so that the aforementioned forgetting can take place

 - or to finely tune the waiting over diascas so that *B is deleted as soon as strictly needed* (i.e. as soon as all potential actors aware of B know now that B is terminating), before even the end of the current tick; in this case the number of diascas to wait depends on the length of the chain of actors knowing B (i.e. actor C may know B and may have transmitted this knowledge to D, etc.)

The first option is by far the simplest and most common: B simply calls ``class_Actor:declareTermination/1``, and, starting from the same diasca, notifies any actor of its deletion. The notification chain will unfold on as many diascas as needed, and once all the diascas for the current tick will be over, a new tick will be scheduled and B will then be deleted automatically.

The second option is more precise but more demanding, as it requires B to be able to determine an upper-bound to the number of diascas that can elapse before it can safely terminate (thus without waiting for the next tick to happen).

Such a feature is provided so that, during a tick, any number of actor creations, deletions and interactions may happen, "instantaneously", and according to any complex pattern.

For example, B may know that only actor C knows it, in which case B will notify C of its termination immediately, implying that starting from ``{ T, D + 2 }`` C is expected to never interact with B anymore (C will receive and process the message at ``{ T, D + 1 }`` but due to message reordering C might already have sent a message to B at this timestamp - in the general case B should ignore it).

In this context B is to call ``class_Actor:declareTermination/2``, with a termination delay of 2 diascas. A larger delay would have to be specified if C had to notify in turn D, and so on...

With both termination options, once ``class_Actor:declareTermination/{1,2}`` is called, the engine will take care of the appropriate waiting and then of the corresponding deletion, with no further intervention.

Note that:

 - should a too short termination delay be chosen by mistake, the simulation engine will do its best to detect it

 - if setting up a proper termination happens to be too cumbersome on to many cases, an automatic system might be designed, in order to keep track of inter-model references (ex: like a garbage collector operated on actors, based on reference counting - either PID or AAI); however this mechanism would probably have some major drawbacks by design (complex, expensive because of reference indirections, etc.); moreover having an implicit, dynamic, flexible communication graph is probably more a feature than a limitation


.. Note::

  The proper termination of an actor results into a *normal* termination, not in a crash. Therefore processes (including other actors) that would be linked to a terminating actor will *not* be terminated in turn because of it.

  On the other hand, as soon as an actor crashes, the simulation is expected to fully crash in turn, in order to avoid silent errors; knowing that anyway no automatic fall-back to a crash can be defined, since it generally means there is a bug in the code of at least a model.




:raw-latex:`\pagebreak`


Summary of The Sim-Diasca Conventions to Enforce
________________________________________________


Regarding State
***************

In the code of an actor (i.e. inheriting from a ``class_Actor`` child instance), the only attributes inherited from Sim-Diasca that should be directly accessed from models is ``trace_categorization``, to provide from the constructor various ways of selecting trace messages afterwards.


All other attributes inherited from a ``class_Actor`` instance should be regarded as strictly private, i.e. as technical details of the engine that are not of interest for the model developer (neither in terms of reading nor of writing).

Of course the developer is free of defining any class hierarchy, with each specialising class defining all (non-colliding) attributes needed.



Regarding Behaviour
*******************

+---------------------------------+--------------------------------------------+------------------------------------+
|Action                           | Correct                                    | Incorrect                          |
|                                 |                                            |                                    |
+=================================+============================================+====================================+
|Initial Actor Creation           | ``class_Actor:create_initial_actor/2``     | Use of a variation of ``spawn`` or |
|(before the simulation start)    | (directly from the simulation case)        | of WOOPER ``new``                  |
+---------------------------------+--------------------------------------------+------------------------------------+
|Runtime Actor Creation           | ``class_Actor:create_actor/3``             | Use of a variation of ``spawn`` or |
|(in the course of the simulation)| (only from another actor)                  | of WOOPER ``new``                  |
+---------------------------------+--------------------------------------------+------------------------------------+
|Actor Communication              | ``class_Actor:send_actor_message/3``       | ``TargetActor ! AMessage``         |
+---------------------------------+--------------------------------------------+------------------------------------+
|Actor Termination Decision       | Notify relevant actors and postpone        | Immediate non-coordinated          |
|                                 | termination until longest possible         | triggered termination              |
|                                 | interaction is necessarily over            |                                    |
+---------------------------------+--------------------------------------------+------------------------------------+
|Actor Termination Execution      | ``class_Actor:declareTermination/{1,2}``   | ``TargetActor ! delete``           |
+---------------------------------+--------------------------------------------+------------------------------------+





:raw-latex:`\pagebreak`


Actor Scheduling
----------------


Basics
......


Simulation Time: Of Ticks and Diascas
_____________________________________


Simulation time is fully decorrelated from wall-clock time, and is controlled by the time manager(s): the ``fundamental frequency`` of the simulation (ex: 50Hz) leads to a unit time-step (a.k.a. ``simulation tick``) to be defined (ex: 20ms, in simulation time), each time-step lasting, in wall-clock time, for any duration needed so that all relevant actors can be evaluated for that tick.

If that wall-clock duration is smaller than the time-step (the simulation is "faster than the clock"), then the simulation can be ``interactive`` (i.e. it can be slowed down on purpose to stay on par with wall-clock time, allowing for example for some human interaction), otherwise it will be in ``batch`` mode (running as fast as possible).

A simulation tick is split into any number of logical moments, named ``diascas``, which are used to solve causality and are not associated to any specific duration by themselves.

Both ticks and diascas are positive unbounded integers.

So a typical simulation timestamp is a tick/diasca pair, typically noted as ``{ T, D }``.


Time Managers
_____________

Controlling this simulation time means offering a scheduling service, here in a distributed way: it relies on a tree of time managers, each being in charge of a set of direct child managers and of local actors.

This scheduling service drives them time-wise, so that they all share the same notion of time (ticks and diascas alike), find a consensus on its flow, while still being able to evaluate all corresponding actors in parallel, in spite of their possible coupling.

In the most general terms, the behaviour of an actor is partly determined by what it would do by itself (its "spontaneous behaviour"), partly by the signals its environment sends to it, i.e. based on the messages that this actors receives from other actors (its "triggered behaviour").

In both cases, for an actor, developing its behaviour boils down to updating its state and/or sending messages to other actors, and possibly planning future spontaneous actions and/or sending information to probe(s).



At Actor Creation
_________________

Each actor, when created, has first its ``onFirstDiasca/2`` actor oneway triggered [#]_. This is the opportunity for this newly created actor to develop any immediate first behaviour, and also to specify at once when it is to be scheduled next for a spontaneous behaviour: otherwise, as all actor are created with an empty agenda, they would remain fully passive (never being spontaneously scheduled), at least until a first actor message (if any) is sent to them.

.. [#] This actor actually receives the corresponding actor message sent by the load balancer, which determined a placement for it and created it.

So all models are expected to define their ``onFirstDiasca/2`` actor oneway [#]_, in which most of them will at least program their next spontaneous schedulings (see, in ``class_Actor``, notably ``addSpontaneousTick/2`` and ``addSpontaneousTicks/2``). This corresponds, internally, to exchanges with the time managers in charge of the corresponding actors.

.. [#] Knowing that the default implementation for ``onFirstDiasca/2``, inherited from ``class_Actor``, simply halts the simulation on error, purposely.

Creations happen at the diasca level rather than at the tick level, so that any sequence of model-related operations (creation, deletion, action and interactions) can happen immediately (in virtual time), to avoid any time bias.



Afterwards
__________


Then a very basic procedure will rule the life of each actor:

 #. when a new simulation tick ``T`` is scheduled, this tick starts at diasca ``D=0``
 #. as the tick was to be scheduled, there was at least one actor which had planned to develop a spontaneous behaviour at this tick; all such actors have their ``actSpontaneous/1`` oneway executed
 #. as soon as at least one actor sent an actor message, the next diasca, ``D+1``, is scheduled [#]_
 #. All actors targeted by such a message (sent at ``D``) process their messages at ``D+1``; possibly they may send in turn other messages
 #. increasing diascas will be created, as long as new actor messages are exchanged
 #. once no more actor message is sent, the tick ``T`` is over, and the next is scheduled (possibly ``T+1``, or any later tick, depending on the spontaneous ticks planned previously)
 #. simulation ends either when no spontaneous tick is planned anymore or when a termination criteria is met (often, a timestamp in virtual time having been reached)

.. [#] Actually there are other reasons for a diasca to be created, like the termination of an actor, but they are transparent for the model developer.


Internally, these scheduling procedures are driven by message exchanges by actors and time managers:

 - when a tick begins (diasca zero), each time manager sends a corresponding message to each of its actors which are to be scheduled for their spontaneous behaviour

 - when a (non-zero) diasca begins, actors that received on the previous diasca at least one actor message are triggered by their time manager, so that each actor can first reorder appropriately its pending messages on compliance with the expected simulation properties (notably: causality, reproducibility, ergodicity), and then process them in turn




Actor Scheduling
................

The basic granularity in virtual time is the tick, further split on as many diascas as needed (logical moments).

The engine is able to automatically:

  - jump over as many ticks as needed: ticks determined to be idle, i.e. in which no actor message is to be processed, are safely skipped

  - trigger only the appropriate actors once a diasca is scheduled, i.e. either the ones which planned a spontaneous behaviour or the ones having received an actor message during the last diasca or being terminating

  - create as many diascas during a tick as strictly needed, i.e. exactly as long as actor messages are exchanged or actors are still terminating


Indeed the simulation engine keeps track both of the sendings of actor messages [#]_ and of the planned future actions for each actor. It can thus determine, once a diasca is over, if all next diascas or even a number of ticks can be safely skipped, and then simply schedule the first next timestamp to come.

.. [#] This is done on a fully distributed way (i.e. through the scheduling tree of time managers over computing nodes) and all communications between an actor and its time manager are purely local (i.e. they are by design on the same Erlang node).

	   Moreover the messages themselves only go from the emitting actor to the recipient one: in each diasca, only the *fact* that the target actor received a first message is of interest, and this is reported only to its own, local time manager - the actual message is never sent to third parties (like a time manager), and no more notifications are sent by the receiving actor once the first message has been reported. So the number of messages, their payload and communication distance are reduced to a bare minimum.



So, for any simulation tick, each actor may or may not be scheduled, and an actor will be scheduled iff:

 - it planned a spontaneous behaviour for this diasca
 - or it received at least one actor message during the last diasca
 - or it is terminating

The actor happens to be itself able to keep track of its expected schedulings, and thus it can automatically check that they indeed match exactly the ones driven by the time manager, for an increased safety.

Anyway these mechanisms are transparent to the model developer, who just has to know that all actor messages, once appropriately reordered, will be triggered on their target, and that the planned spontaneous schedulings will be enforced by the engine, according to the requests of each actor.

Thus the developer just has to define the various actor oneways that the model should support (i.e. the ones that other actors could trigger thanks to an actor message), and the spontaneous behaviour of that model (i.e. its ``actSpontaneous/1`` oneway). Then the simulation engine takes care of the rest.



Planning Future Spontaneous Behaviour
.....................................

Each actor is able to specify, while being scheduled for any reason (an actor message having been received, and/or a spontaneous action taking place), at least one additional tick at which it should be spontaneously scheduled later. An actor can be scheduled for a spontaneous action up to once per tick.

To do so, it can rely on a very simple API, defined in ``class_Actor``:

 - ``scheduleNextSpontaneousTick/1``: requests the next tick to be added to the future spontaneous ticks of this actor

 - ``addSpontaneousTick/2``: adds the specified spontaneous tick offset to the already registered ones

 - ``addSpontaneousTicks/2``: same as before, this time for a *list* of tick offsets

 - ``withdrawSpontaneousTick/2``: withdraws the specified spontaneous tick offset from the already registered ones

 - ``withdrawSpontaneousTicks/2``: same as before, this time for a *list* of tick offsets


An actor may also decide instead to terminate, using ``declareTermination/{1,2}`` for that, once having withdrawn any spontaneous ticks that it had already planned [#]_.

.. [#] The time management service could be able to determine by itself which ticks shall be withdrawn whenever an actor departs, however this operation would not be scalable at all (it would become prohibitively expensive as soon as there are many actors and/or many ticks planned for future actions).




Data Management
---------------

In a distributed context, on each computing host, the current working directory of the simulation is set automatically to a temporary root directory, which will be appropriately cleaned-up and re-created.

This root directory is in ``/tmp``, to store all live data, deployed for the simulation or produced by it.

Its name starts with ``sim-diasca`` (to prevent clashes with other applications), then continues with the name of the simulation case (so that multiple cases can run in the same context), then finishes with the user name (so that multiple users can run the same cases on the same hosts with no interference).

Thus the root directory of a simulation on any host is named like::

  /tmp/sim-diasca-<name of the simulation case>-<user name>

For example::

  /tmp/sim-diasca-Sim-Diasca_Soda_Integration_Test-boudevil

This root directory has two sub-directories:

 - ``deployed-elements``, which corresponds to the content of the simulation package (i.e. both code and data, both for the engine and for the third-party elements, if any)
 - ``outputs``, which is to contain all live data produced by the simulation (ex: data file, probe reports, etc.); all computing nodes will have directly this directory as working (current) directory


A simulator which added third-party data to the simulation archive (thanks to the ``additional_elements_to_deploy`` field of the deployment settings specified in the simulation case) is able to access to them thanks to ``class_Actor:get_deployed_root_directory/1``.


For example, if the following was specified::

  DeploymentSettings = #deployment_settings{
	...
	additional_elements_to_deploy = [
	  {"mock-simulators/soda-test",code},
	  {"mock-simulators/soda-test/src/soda_test.dat",data}
	...
  },
  ...


Then all models are able to access to the data file thanks to::

  DataPath = file_utils:join( class_Actor:get_deployed_root_directory(State),
	"mock-simulators/soda-test/src/soda_test.dat" ),
  % Then open, read, parse, etc. at will.


On simulation success, all results will be appropriately generated (in a rather optimal, parallel, distributed way), then aggregated and sent over the network to the centralised result directory, created in the directory from which the simulation was launched, on the user host.

Finally, on simulation shutdown, the deployment base directory will be fully removed.
