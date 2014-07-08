:raw-latex:`\pagebreak`


----------------------
Sim-Diasca Cheat Sheet
----------------------

.. warning:: This section is to be updated.


Note: unless specified otherwise, mentioned tests are to be found in the ``sim-diasca/src/core/src/scheduling/tests`` directory.




Which Sim-Diasca Version Should Be Used?
========================================

There are two main Sim-Diasca branches:

 - the centralised one (1.x branch)
 - the distributed one (2.x branch)


The APIs they expose to the models tend to converge over time. Currently their main difference is that the distributed branch supports advanced scheduling (actors being able to specify arbitrary activation ticks), whereas the centralised version does not support it yet.

More generally, the centralised versions are simpler, more lightweight and start faster than the distributed ones (ex: they do not require a deployment phase to be performed), thus they could be preferred when developing models, since allowing shorter write/test/debug cycles.

The distributed versions are of course the most appropriate ones whenever having to launch real-size simulations.




How Actors Are To Be Created?
=============================

Actors are to be created either before the simulation starts (they are then called *initial actors*) or in the course of the simulation (*simulation-time actors*).

In all cases, their creation must be managed through the simulation engine, not directly by the user (ex: using ``erlang:spawn*`` or any WOOPER ``new`` variation is *not* allowed), as otherwise even essential simulation properties could not be preserved.

An actor can be either automatically created by the engine on a computing node chosen according to its default heuristic (agnostic placement), or the target node can be selected according to a *placement hint*, specified at the actor creation. In the latter case, the engine will then do its best to place all actors being created with the same placement hint on the same computing node, to further optimise the management of tightly coupled actors.


Initial Actors
--------------

Initial actors are to be created directly from the simulation case, and their creation must be synchronous, otherwise there could be a race condition between the moment they are all up and ready and the moment at which the simulation starts.

There must be at least one initial actor, as otherwise the simulation will stop as soon as started, since it will detect that no event at all can possibly happen anymore.


With Agnostic Actor Placement
.............................


The actual creation is in this case done thanks to the ``class_Actor:create_initial_actor/2`` static method, whose API is identical in the centralised and distributed branches.

For example, if wanting to create an initial soda vending machine (``class_SodaVendingMachine``), whose constructor takes two parameters (its name and its initial stock of cans), then one has simply to use, before the simulation is started::

	...
	VendingMachinePid = class_Actor:create_initial_actor(
	   class_SodaVendingMachine, [ _Name="My machine", _CanCount=15 ] ),
	...
	% Now simulation can be started.


An additional static method, ``class_Actor:create_initial_actor/3``, is available, the third parameter being the PID of an already-retrieved load balancer. This allows, when creating a large number of initial actors, to retrieve the load balancer once for all, instead of looking it up again and again, at each ``class_Actor:create_initial_actor/2`` call.


For example::

  ...
  LoadBalancerPid = class_LoadBalancer:get_balancer(),
  ...

  FirstVendingMachinePid = class_Actor:create_initial_actor(
	   class_SodaVendingMachine, [ _Name="My first machine",
		  _FirstCanCount=15 ],
	   LoadBalancerPid ),
  ...
  SecondVendingMachinePid = class_Actor:create_initial_actor(
	   class_SodaVendingMachine, [ "My second machine",
		  _SecondCanCount=8 ],
	   LoadBalancerPid ),
  ...
  % Now simulation can be started.



Full examples can be found in:

 - ``scheduling_one_initial_terminating_actor_test.erl``
 - ``scheduling_one_initial_non_terminating_actor_test.erl``



Based On A Placement Hint
.........................

The same kind of calls as previously can be used, with an additional parameter, which is the placement hint, which can be any Erlang term chosen by the developer.

In the following example, first and second vending machines should be placed on the same computing node (having the same hint), whereas the third vending machine may be placed on any node::

  ...
  FirstVendingMachinePid = class_Actor:create_initial_placed_actor(
	class_SodaVendingMachine, [ "My first machine", _CanCount=15 ]
	my_placement_hint_a ),
  ...
  % Using now the variation with an explicit load balancer:
  % (only available in the distributed case)
  LoadBalancerPid = class_LoadBalancer:get_balancer(),
  ...

  SecondVendingMachinePid = class_Actor:create_initial_placed_actor(
	   class_SodaVendingMachine, [ "My second machine",
		 _SecondCanCount=0 ],
	   LoadBalancerPid, my_placement_hint_a ),
  ...
  ThirdVendingMachinePid = class_Actor:create_initial_actor(
	   class_SodaVendingMachine, [ "My third machine",
		 _ThirdCanCount=8 ],
	   LoadBalancerPid, my_placement_hint_b ),
  ...
  % Now simulation can be started.


In a centralised version, placement hints are simply ignored.

Full examples can be found in ``scheduling_initial_placement_hint_test.erl``.



Simulation-Time Actors
----------------------

These actors are created in the course of the simulation.

Such actors can *only* be created by other (pre-existing) actors, otherwise the uncoupling of real time and simulated times would be jeopardised. Thus once the simulation is started it is the only way of introducing new actors.

As before, actors can be created with or without placement hints.


With Agnostic Actor Placement
.............................

An actor A needing to create another one (B) should use the ``class_Actor:create_actor/3`` helper function.

For example::

   ...
   CreatedState = class_Actor:create_actor(
		_CreatedClassname=class_PinkFlamingo,
		[ _Name="Ringo", _Age=34 ], CurrentState ),
   ...


If actor A calls this function at a simulation timestamp {T,D}, then B will be created at the next diasca (hence at {T,D+1}) and A will be notified of it at {T,D+2}.

Indeed the load balancer will process the underlying actor creation message (which is an actor oneway) at {T,D+1} and will create immediately actor B, whose PID will be notified to A thanks to another actor oneway, ``onActorCreated/5``, sent on the same diasca. This message will then be processed by A at {T,D+2}, for example::

   onActorCreated( State, CreatedActorPid,
				ActorClassName=class_PinkFlamingo,
				ActorConstructionParameters=[ "Ringo", 34 ],
				LoadBalancerPid ) ->
   % Of course this oneway is usually overridden, at least
   % to record the PID of the created actor and/or to start
   % interacting with it.




Based On A Placement Hint
.........................

An actor A needing to create another one (B) while specifying a placement hint should simply use the ``class_Actor:create_placed_actor/4`` helper function for that.

Then the creation will transparently be done according to the placement hint, and the ``onActorCreated/5`` actor oneway will be triggered back on the side of the actor which requested this creation, exactly as in the case with no placement hint.


How Constructors of Actors Are To Be Defined?
=============================================

Actor classes are to be defined exactly like any WOOPER classes (of course they have to inherit, directly or not, from ``class_Actor``), except that their first construction parameter must be their actor settings.

These settings (which include the actor's AAI, for *Abstract Actor Identifier*) will be specified automatically by the engine, and should be seen as opaque information just to be transmitted to the parent constructor(s).

All other parameters (if any) are call *actual parameters*.

For example, a ``class_Foo`` class may define its WOOPER construct parameters as::

  -define( wooper_construct_parameters, ActorSettings,
	 FirstParameter, SecondParameter ).


If this class had taken no specific actual construction parameter, we would have had::

  -define( wooper_construct_parameters, ActorSettings ).


The creation of an instance will require all actual parameters to be specified by the caller (since the actor settings will be determined and assigned by the simulation engine itself).

For example::

  ...
  MyFooPid = class_Actor:create_initial_actor( class_Foo,
	[ MyFirstParameter, MySecondParameter] ),
  % Actor settings will be automatically added at creation-time
  % by the engine.

For a complete example, see ``class_TestActor.erl``.



How Actors Can Define Their Spontaneous Behaviour?
==================================================

They just have to override the default implementation of the ``class_Actor:actSpontaneous/1`` oneway.

The simplest of all spontaneous behaviour is to do nothing at all::

  actSpontaneous(State) ->
	 State.

For a complete example, see ``class_TestActor.erl``.



How Actors Are To Interact?
===========================

Actors must *only* interact based on ``actor messages`` (ex: using Erlang messages or WOOPER ones is *not* allowed), as otherwise even essential simulation properties could not be preserved.

Thus the ``class_Actor:send_actor_message/3`` helper function should be used for each and every inter-actor communication (see the function header for a detailed usage information).

As a consequence, only actor oneways are to be used, and if an actor A sends an actor message to an actor B at simulation timestamp {T,D}, then B will process it at tick {T,D+1}, i.e. at the next diasca (that will be automatically scheduled).

Requests, i.e. a message sent from an actor A to an actor B (the question), to be followed by a message being sent back from B to A (the answer), must be implemented based on a round-trip exchange of actor oneways.

For example, if actor A wants to know the color of actor B, then:

 - first at tick T, diasca D, actor A sends an actor message to B, ex: ``SentState = class_Actor:send_actor_message( PidOfB, getColor, CurrentState ), ...`` (probably from its ``actSpontaneous/1`` oneway)

 - then, at diasca D+1, the ``getColor(State,SenderPid)`` oneway of actor B is triggered, in the body of which B should send, as an answer, a second actor message, back to A: ``AnswerState = class_Actor:send_actor_message(SenderPid, {beNotifiedOfColor,red}, CurrentState)``; here ``SenderPid`` corresponds to the PID of A and we suppose that the specification requires the answer to be sent immediately by B (as opposed to a deferred answer that would have to be sent after a duration corresponding to some number of ticks)

 - then at diasca D+2 actor A processes this answer: its ``beNotifiedOfColor( State, Color, SenderPid )`` oneway is called, and it can react appropriately; here ``Color`` could be ``red``, and ``SenderPid`` corresponds to the PID of B


Finally, the only licit case involving the direct use of a WOOPER request (instead of an exchange of actor messages) in Sim-Diasca occurs before the simulation is started.

This is useful typically whenever the simulation case needs to interact with some initial actors [#]_ or when two initial actors have to communicate, in both cases *before* the simulation is started.

.. [#] For example requests can be used to set up the connectivity between initial actors, i.e. to specify which actor shall be aware of which, i.e. shall know its PID.



How Actors Are To Be Deleted?
=============================

Actors are to be deleted either in the course of the simulation or after the simulation is over.

In all cases their deletion must be managed through the simulation engine, not directly by the user (ex: sending  WOOPER ``delete`` messages is *not* allowed), as otherwise even essential simulation properties could not be preserved.

The recommended way of deleting an actor is to have it trigger its own deletion process. Indeed this requires at least that actor to notify all other actors that may interact with it that this should not happen anymore.

Once they are notified, this actor (possibly on the same tick at which it sent these notifications) should execute its ``declareTermination/{1,2}`` oneway (or the ``class_Actor:declare_termination/{1,2}`` helper function), for example from  ``actSpontaneous/1``::

  ...
  TerminatingState = executeOneway( CurrentState,  declareTermination),
  ...


See ``class_TestActor.erl`` for an example of complex yet proper coordinated termination, when a terminating actor knows other actors and is known by other actors.

See also the ``Sim-Diasca Developer Guide``.



How Requests Should Be Managed From A Simulation Case?
======================================================

As already explained, direct WOOPER calls should not be used to modify the state of the simulation once it has been started, as we have to let the simulation layer have full control over the exchanges, notably so that they can be reordered.

However requests can be used *before* the simulation is started.

For example we may want to know, from the simulation case, what the initial time will be, like in::

	TimeManagerPid ! {getTextualTimings,[],self()},
	receive

		{wooper_result,TimingString} when is_list(TimingString) ->
			?test_info_fmt("Initial time is ~s.",[TimingString])

	end,
	...


The ``is_list/1`` guard would be mandatory here, as other messages may spontaneously be sent to the simulation case [#]_. 


.. [#] Typically the trace supervisor will send ``{wooper_result,monitor_ok}`` messages to the simulation case whenever the user closes the window of the trace supervision tool, which can happen at any time: without the guard, we could then have  ``TimingString`` be unfortunately bound to ``monitor_ok``, instead of the expected timing string returned by the ``getTextualTimings`` request.


However, specifying, at each request call issued from the simulation case, a proper guard is tedious and error-prone, so a dedicated, safe function is provided for that by the engine, ``test_receive/0``; thus the previous example should be written that way instead::

	TimeManagerPid ! {getTextualTimings,[],self()},
	TimingString = test_receive(),
	?test_info_fmt("Received time: ~s.",[TimingString]),
	...


This ``test_receive/0`` function performs a (blocking) selective receive, retrieving any WOOPER result which is *not* emanating directly from the operation of the engine itself. That way, developers of simulation cases can reliably retrieve the values returned by the requests they send, with no fear of interference.



How Should I run larger simulations?
====================================

If, for a given simulation, more than a few nodes are needed, then various preventive measures shall be taken in order to be ready to go to further scales (typically disabling most traces_, extending key time-outs, etc.).

For that the ``EXECUTION_TARGET`` compile-time overall flag has been defined. Its default value is ``development`` (simulations will not be really scalable, but a good troubleshooting support will be provided), but if you set it to ``production``, then all settings for larger simulations will be applied.

It is a compile-time option, hence it must be applied when building Sim-Diasca and the layers above; thus one may run::

  make clean all EXECUTION_TARGET=production

to prepare for any demanding run.

One may instead set ``EXECUTION_TARGET=production`` once for all, typically in ``common/GNUmakevars.inc``, however most users prefer to go back and forth between the execution target settings.
