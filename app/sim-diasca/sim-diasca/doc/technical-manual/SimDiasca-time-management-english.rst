:raw-latex:`\pagebreak`


------------------------------------
Sim-Diasca Time Management Explained
------------------------------------

:raw-html:`<img src="xkcd-time_management.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-time_management.png}`


As already mentioned, the approach to the management of simulation time is most probably the key point of the design of a parallel, distributed simulation engine like the one discussed here.

There are several options to manage time, and, depending on the simulation properties that are needed, some methods are more appropriate than others.

Their main role is to uncouple the *simulation* time (i.e. the virtual time the model instances live in) from the *wall-clock* time (i.e. the actual time that we, users, experience), knowing that the powerful computing resources available thanks to a parallel and distributed context may only be used at the expense of some added complexity at the engine level.



Preservation of Properties
==========================


Maintaining Causality
---------------------

In the context of a distributed simulation, should no special mechanism be used, the propagation of simulated events would depend on the propagation of messages in the actual network of computing nodes, which offers no guarantee in terms of respective timing and therefore order of reception.

As the minimal example below [#]_ shows, a simulation cannot work correctly as such:

:raw-html:`<img src="causality-issues-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{causality-issues-english.png}`

.. [#] The military setting is due to the fact their simulations have been ahead of civil ones for long.

There are three simulation actors here, which are supposed to be instantiated each on a different computing node. Thus, when they communicate, they exchange messages over the network on which the distributed simulator is running.

Actor #1 is a rocket launcher that fires to actor #2, which is a tank. Thus actor #1 sends a message, M1, to all simulation actors that could be interested by this fact (including actor #2 and actor #3), to notify them of the rocket launch.

Here, in this technical context (computers and network), actor #2 (the tank) happens to receive M1 before actor #3 (the observer).

According to its model, the tank, when hit by a rocket, must explode. Therefore it sends a message (M2) to relevant actors (among which there is the observer), to notify them it exploded and that the corresponding technical actor is removed from the simulation.

The problem lies in the point of view of actor #3. Indeed in that case the observer received:

 1. M2, which told it that a tank exploded for no reason (unexpected behaviour)
 2. then M1, which tells it that a rocket was fired to a simulation actor that actually does not even exist

This situation makes absolutely no sense for this actor #3. At best, the model of the observer should detect the inconsistency and stop the simulation. At worse, the actor received incorrect inputs, and in turn injected incorrect outputs in a simulation that should not be trusted anymore.

:raw-html:`<img src="xkcd-protocol.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-protocol.png}`

The root of the problem is that here there is no guarantee that received messages will respect their timely constraints - whereas (at least in synchronous approaches) no return to the past shall be considered, however tempting.

:raw-html:`<img src="xkcd-the_past.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-the_past.png}`


This faulty behaviour would be all the more unfortunate that the incorrect outputs are likely to be indistinguishable from correct ones (i.e. they can go unnoticed in the simulation), distorting the results invisibly, a bit like a pocket calculator which would silently ignore parentheses, and would nevertheless output results that look correct, but are not.


Maintaining Reproducibility
---------------------------

:raw-html:`<img src="xkcd-the_difference.png"></img>`
:raw-latex:`\includegraphics[scale=4.5]{xkcd-the_difference.png}`

Let's suppose for now we somehow managed to preserve causality. This does not imply that reproducibility is ensured.

Using the same example where actor #1 launches a rocket (sending the M1 message), actor #3 can in the meantime develop its own behaviour, which may imply this observer detected the tank. This can lead the observer notifying the tank, thus to its sending the M3 message.

:raw-html:`<img src="reproducibility-issues-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.65]{reproducibility-issues-english.png}`

The point here is that there is no direct nor causal relationship between M1 and M3. These are truly concurrent events, they may actually happen in any order. Therefore concurrent events are not expected to be reordered by the mechanism used to maintain causality, since situations A and B are equally correct.

However, when the user runs twice exactly the same simulation, she most probably expects to obtain the same result [#]_: here M1 should be received by actor #2 *always* before M3, or M3 *always* before M1, and the implicit race condition should not exist in that context.

.. [#] Otherwise she would not be able to interpret the consequences of a change in the simulation parameters unless she runs thousands of simulations to monitor macroscopic values only, instead of running two simulations (with and without the change) and comparing just the outcome of these two particular trajectories of the system.

In that case, causality is not enough, some additional measures have to be taken to obtain reproducibility as well.

With some time management approaches, once causality is restored, ensuring reproducibility is only a matter of enforcing an arbitrary order (i.e. which depends only on these messages, not in any way on the context) on concurrent events.



Allowing For Ergodicity
-----------------------

The context-free message reordering allows to recreate the arbitrary order we need to ensure reproducibility.

However the simulator should offer the possibility to go beyond this mechanism, otherwise "ergodicity" (a term we chose in reference to Monte-Carlo computations) cannot be achieved: in some cases we want all combinations of concurrent events to be able to occur, not only the ones that correspond to the arbitrary order we enforced.


.. Note:: Just disabling the reproducibility mechanism would not be a solution: if no reordering at all was enabled, the natural sequence of concurrent events (which would then be dictated by the computing infrastructure) would not guarantee any ergodicity; some sequences of events would happen a lot more frequently than others, although they should not.


The best solution we know here is, in a time-stepped context, to let the reproducibility mechanism activated, but, in addition to the sorting into an arbitrary order, to perform then an uniform random shuffle: then we are able not only to recreate *all* licit combinations of events during a given simulation tick at the level of each actor, but also to ensure that all these combinations have *exactly* the same probability of showing up.


:raw-html:`<img src="ergodicity-issues-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{ergodicity-issues-english.png}`



:raw-latex:`\pagebreak`


Approaches to Time Management
=============================

As far as we know, there are mainly four ways of managing time correctly, in a distributed context, in the context of a simulation in discrete time.


Approach A: use of a centralised queue of events
------------------------------------------------

A unique centralised queue of simulation events is maintained, events are sorted chronologically, and processed one after the other.

Pros:

 - purely sequential, incredibly simple to implement


Cons:

 - splendid bottleneck, not able to scale at all, no concurrent processing generally feasible, distribution not really usable there; would be painfully slow on most platforms as soon as more than a few hundreds of models are involved



Approach B: use a time-stepped approach
---------------------------------------

The simulation time is chopped in intervals short enough to be able to consider the system as a whole as constant during a time step, and the simulator iterates through time-steps.

Pros:

 - still relatively simple to understand and implement

 - may allow for a massive, yet rather effective, parallelization of the evaluation of model instances

 - the simulation engine may be able to automatically jump over ticks that are identified as being necessarily idle, gaining most of the advantages of event-driven simulations

 - the resulting simulator can work in batch mode or in interactive mode with very small effort, and with no real loss regarding best achievable performance


Cons:

 - not strictly as simple as one could think, but doable (ex: reordering of events must be managed, management of stochastic values must be properly thought of, induced latency may either add some constraints to the implementation of models or require a more complex approach to time management)

 - a value of the time step must be chosen appropriately (although we could imagine that advanced engines could determine it automatically, based on the needs expressed by each model)



Approach C: use a *conservative* event-driven approach
------------------------------------------------------

The simulation time will not advance until all model instances know for sure they will never receive a message from the past.


Pros:

 - efficient for systems with only few events occurring over long periods

 - there must be other advantages (other than the fact it is still a field of actual academic research) that I overlooked or that do not apply to the case discussed here


Cons:

 - complex algorithms are needed: it is proven that this mechanism, in the general case, leads to deadlocks. Thus a mechanism to detect them, and another one to overcome them, must be implemented

 - deadlock management and attempts of avoidance induce a lot of null (empty) messages to be exchanged to ensure that timestamps make progress, and this generally implies a significant waste of bandwidth (thus slowing down the whole simulation)



Approach D:	use an *optimistic* event-driven approach
-----------------------------------------------------

For each model instance (actor), simulation time will advance carelessly, i.e. disregarding the fact that other model instances might not have reached that point in time yet.

Obviously it may lead to desynchronised times across actors, but when such an actor receives a message from its past, it will rewind its logic and state in time and restart from that past. The problem is that it will likely have sent messages to other actors in-between, so it will have to send anti-messages that will lead to cascading rewinds and anti-messages...

Pros:

 - efficient in some very specific situations where actors tend to nevertheless advance spontaneously at the same pace, thus minimising the number of messages in the past received (not the case here, I think)

 - there must be other advantages (other than the fact it is still a field of actual academic research) that I overlooked or that do not apply to the case discussed here


Cons:

 - overcoming messages in the past implies developing a generic algorithm allowing for distributed roll-backs over a graph of model instances. This is one of the most complex algorithm I know and for sure I would not dare to implement and validate it, except maybe in a research-oriented project

 - even when correctly implemented, each simulation roll-back is likely to be a real performance killer





:raw-latex:`\pagebreak`


Sim-Diasca Time Management Algorithm
====================================


General Principles
------------------


:raw-html:`<img src="xkcd-debugger.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-debugger.png}`


Sim-Diasca is based on approach B, i.e. it uses a synchronous (discrete time, *time-stepped*) algorithm for time management.

It has been deemed to be the most interesting trade-off between algorithmic complexity and scalability of the result. The manageable complexity of this approach allowed to bet on a rather advanced scheduler, featuring notably:

 - massive scalability, thanks to a fully parallel and distributed mode of operation yet with direct actor communication (i.e. inter-actor messages are never collected into any third-party agenda)

 - the ability to automatically jump over any number of idle ticks

 - the "zero time bias" feature, allowing to avoid any model-level latency in virtual time (causality solving does not make the simulation time drift)


The simplicity of approach A was surely tempting, but when it evaluates one model instance at a time, the other approaches can potentially evaluate for example 35 millions of them in parallel. Fearless researchers might go for C or D. Industrial feedback about approach B was encouraging.


Simplified Mode of Operation
----------------------------


A time step will be generally mentioned here as a *simulation tick*.

Sim-Diasca uses a special technical component - a process with a specific role - which is called the **Time Manager** and acts as the simulation scheduler.

It will be the sole controller of the overall simulation time. Once created, it is notably given:

 - a simulation start time, for example: ``Thursday, November 13, 2008 at 3:19:00 PM``, from which the initial simulation tick will be deduced

 - an operating frequency, for example: 50 Hz, which means each virtual second will be split in 50 periods, with therefore a (constant) simulation tick whose duration - in virtual time - will be ``1000/50 = 20 ms``; this time step must be chosen appropriately, depending on the system to simulate [#]_

 - an operating mode, i.e. batch or interactive

.. [#] Currently 50 Hz has been the highest frequency that was deemed useful for our application cases, knowing that this corresponded to a device scheduled by the 50 Hz electric power transmission.


In batch mode, the simulation will run as fast as possible, whereas in interactive mode, the simulator will be kept on par with the user (wall-clock) time. If the simulation fails to do so (i.e. if it cannot run fast enough), the user is notified of the scheduling failure, and the simulator tries to keep on track, on a best-effort basis.

Not depending on the operating mode, when started the ``Time Manager`` will always follow the same algorithm, shown below:

:raw-html:`<img src="tick-timescale-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{tick-timescale-english.png}`

At the beginning of a new tick, the ``Time Manager`` will notify all subscribed simulation actors that a new tick began, thanks to a ``top`` message.

Each actor will then process all the actor messages it received during the last tick, reordered appropriately, as explained in the `Maintaining Causality`_ and `Maintaining Reproducibility`_ sections. This deferred message processing ensures the simulation time always progresses forward, which is a property that simplifies considerably the time management.

:raw-html:`<img src="xkcd-time_machines.png"></img>`
:raw-latex:`\includegraphics[scale=0.4]{xkcd-time_machines.png}`

Processing these actor messages may imply state changes in that actor and/or the sending of actor messages to other actors.

Once all past messages have been processed, the actor will go on, and act according to its spontaneous behaviour. Then again, this may imply state changes in that actor and/or the sending of actor messages to other actors.

Finally, the actor reports to the time manager that it finished its simulation tick, thanks to a ``done`` message.

The key point is that all actors can go through these operations *concurrently*, i.e. there is no limit on the number of actors that can process their tick simultaneously.

Therefore each simulation tick will not last longer than needed, since the time manager will determine that the tick is over as soon as the last actor reported it has finished processing its tick.

More precisely, here each simulation tick will last no longer than the duration took by the actor needing the most time to process its tick, compared to a centralised approach where it would last as long as the sum of all the durations needed by each actor. This is a tremendous speed-up indeed.

Then the time manager will determine that the time for the next tick has come.



Actual Mode of Operation
------------------------

For the sake of clarity, the previous description relied on quite a few simplifications, that are detailed here.



Distributed Mode Of Operation
.............................

The scheduling service has been presented as if it was centralised, which is not the case: it is actually fully distributed, based on a hierarchy of ``Time Manager`` instances.

Indeed they form a scheduling tree, each time manager being able to federate any number of child managers and of local actors. They recursively establish what is the next tick to schedule, each based on its own sub-hierarchy. The root time manager is then able to determine what is the next overall tick which is to be scheduled next (jumping then directly over idle ticks).

The current deployment setting is to assign exactly one time manager per computing host, and, for a lower latency, to make all time managers be direct children of the root one (thus the height of the default scheduling tree is one).

Other settings could have been imagined (balanced tree of computing hosts, one time manager per processor or even per core - rather than one per host, etc.).



Actual Fine-Grain Scheduling
............................

The simulation time is discretised into fundamental time steps (``ticks``, which are positive, unbounded integers) of equal duration in virtual time (ex: 10ms, for a simulation running at 100 Hz) that are increased monotonically.


From the user-specified simulation start date (ex: ``Monday, March 10, 2014 at 3:15:36 PM``), a simulation initial tick ``Tinitial`` is defined (ex: ``Tinitial = 6311390400000``).

:raw-html:`<img src="xkcd-unique_date.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-unique_date.png}`


``Tick offsets`` can be used instead of absolute ticks; these offsets are defined as a difference between two ticks, and represent a duration (ex: at 100Hz, ``Toffset=15000`` will correspond to a duration of 2 minutes and 30 seconds in virtual time).

Internally, actors use mostly tick offsets defined relatively to the simulation initial tick.

During a tick T, any number of logical moments (``diascas``, which are positive, unbounded integers) can elapse. Each tick starts at diasca D=0, and as many increasing diascas as needed are created to solve causality.

All diascas of a tick occur at the same simulation timestamp (which is this tick), they solely represent logical moments into this tick, linked by an "happen before" relationship: if two events E1 and E2 happen respectively at D1 and D2 (both at the tick T), and if D1 < D2, then D1 happened before D2.

So the full timestamp for an event is a Tick-Diasca pair, typically noted as ``{T,D}``.

Diascas allows to manage causality despite parallelism: effects will always happen *strictly later* than their cause, i.e. at the very least on the diasca immediately following the one of the cause, if not in later diascas or even ticks, depending on the intended timing of the causal mechanism: causes can follow effects either immediately or after any specified duration in virtual time [#]_.

This is what happens when an actor A1 sends a message to an actor A2 at tick T, diasca D (i.e. at {T,D}). A2 will process this message at {T,D+1}. If needing to send back an answer, it may do it directly (while still at {T,D+1}), and A1 will be able to process it at {T,D+2}.

This allows immediate exchanges in virtual time (we are still at tick T - and A2 could have similarly triggered any number of third parties before answering to A1, simply resulting in an increase of the current diasca), while still being massively parallel and preserving causality and other expected simulation properties. Of course non-immediate exchanges are also easily done, since A2 may wait for any number of ticks before sending its answer to A1.



.. [#] Durations shall been specified by modellers regardless of a simulation frequency, in absolute terms (ex: "6 minutes and 20 seconds"), rather than directly as a number of ticks: the engine is indeed able to convert the former to the latter at runtime, and to stop automatically if the conversion resulted in a rounding error higher than a threshold (either the default one, or a user-specified one for that duration). As much as possible, models should be uncoupled from the simulation frequency.


Consensus on the End of Tick
............................

There must be a consensus between the actors to decide whether the current tick can be ended. One of the most effective way of obtaining that consensus is to rely on an arbitrator (the ``Time Manager``) *and* to force the acknowledgement of all actor messages, from the recipient to the sender.

In the lack of such of an acknowledgement, if, at tick T, an actor A1 sent a message M to an actor A2, which is supposed here to have already finished its tick, and then sent immediately a ``done`` message to the ``Time Manager`` (i.e. without waiting for an acknowledgement from A2 and deferring its own end of tick), then there would exist a race condition for A2 between the message M and the ``top`` notification of the ``Time Manager`` for tick T+1.

There would exist no guarantee that M was received before the next ``top`` message, and therefore the M message could be wrongly interpreted by A2 as being sent from T+1 (and thus processed in T+2), whereas it should be processed one tick earlier.

This is the reason why, when an actor has finished its spontaneous behaviour, it will:

 - either end its tick immediately, if it did not send any actor message this tick

 - or wait to have received all pending acknowledgements corresponding to the actor messages it sent this tick, before ending its tick



Scheduling Cycle
................

Before interacting with others, each actor should register first to the time manager. This allows to synchronise that actor with the current tick and then notify it when the first next tick will occur.

At the other end of the scheduling cycle, an actor should be able to be withdrawn from the simulation, for any reason, including its removal decided by its model.

To do so, at the end of the tick, instead of sending to the ``Time Manager`` a ``done`` message, it will send a ``terminated`` message. Then the time manager will unregister that actor, and during the next tick it will send it its last ``top`` message, upon which the actor will be considered allowed to be de-allocated.

.. Note:: The removal cannot be done during the tick where the actor sent its ``terminated`` message, as this actor might still receive messages from other actors that it will have to acknowledge, as explained in the previous section.

As for the management of the time manager itself, it can be started, suspended, resumed, stopped at will.



Criterion for Simulation Ending
...............................

Once started, a simulation must evaluate on which condition it should stop. This is usually based on a termination date (in virtual time), or when a model determines that an end condition is met.


Need for Higher-Level Actor Identifiers
.......................................

When actors are created, usually the underlying software platform (ex: the multi-agent system, the distribution back-end, the virtual machine, the broker, etc.) is able to associate to each actor a unique *technical distributed identifier* (ex: a platform-specific process identifier, a networked address, etc.) which allows to send messages to this actor regardless of the location where it is instantiated.

However, as the reordering algorithms rely - at least partly - onto the senders of the messages to sort them, the technical distributed identifiers are not enough here.

Indeed, if the same simulation is run on different sets of computers, or simply if it runs on the same computers but with a load-balancer which takes into account the effective load of the computing nodes, then, from a run to another, the same logical actor may not be created on the same computer, and hence may have a different technical distributed identifier, which in turn will result in different re-orderings being enforced and, finally, different simulation outcomes to be produced, whereas for example reproducibility was wanted.

Therefore higher-level identifiers must be used, named here *actor identifiers*, managed so that their value will not depend on the technical infrastructure.

Their assignment is better managed if the load balancer take care of them.

On a side note, this actor identifier would allow to implement dynamic actor migration quite easily.



Load-balancing
..............

Being able to rely on a load balancer to create actors over the distributed resources allows to run simulations more easily (no more hand-made dispatching of the actors over varying sets of computers) and, with an appropriate placing heuristic, more efficiently.

Moreover, as already mentioned, it is the natural place to assign actor identifiers.

The usual case is when multiple actors (ex: deployment policies) need to create new actors simultaneously (at the same tick).

In any case the creating actors will rely on the engine-provided API (ex: in ``class_Actor``, for creations in the course of the simulation, ``create_actor/3`` and ``create_placed_actor/4`` shall be used), which will result in sending actor messages to the load balancer, which is itself a (purely passive) actor, scheduled by a time manager. These creation requests will be therefore reordered as usual, and processed one by one.

As for initial actor creations, still in ``class_Actor``, different solutions exist as well:

 - ``create_initial_actor/{2,3}``, for a basic creation with no specific placement
 - ``create_initial_placed_actor/{3,4}``, for a creation based on a placement hint
 - ``create_initial_actors/{1,2}``, for an efficient (batched and parallel) creation of a (potentially large) set of actors, possibly with placement hints


When the load balancer has to create an actor, it will first determine what is the best computing node on which the actor should be spawned. Then it will trigger the (synchronous and potentially remote) creation of that actor on that node, and specify what its Abstract Actor Identifier (AAI) will be (it is simply an integer counter, incremented at each actor creation).

As the operation is synchronous, for single creations the load balancer will wait for the actor until it has finished its first initialisation phase, which corresponds to the technical actor being up and ready, for example just having finished to execute its constructor.

Then the load balancer will have finished its role for that actor, once having stored the association between the technical identifier (PID) and the actor identifier (AAI), for later conversion requests (acting a bit like a naming service).



Actor - Time Manager Relationships
..................................

We have seen how a load balancer creates an actor and waits for its construction to be over.

During this phase, that actor will have to interact with its (local) time manager: first the actor will request the scheduling settings (ex: what is the fundamental simulation frequency), then it will subscribe to its time manager (telling it how it is to be scheduled: step by step, passively, periodically, etc.), which will then answer by specifying all the necessary information for the actor to enter the simulation: what will be the current tick, whether the simulation mode targets reproducibility or ergodicity (in this latter case, an appropriate seed will be given to the actor), etc.

These exchanges will be based on direct (non-actor) messages, as their order does not matter and as they all take place during the same simulation tick, since the load balancer is itself a scheduled actor that will not terminate its tick as long as the actors have not finished their creation.



Related agents
..............


Time managers (implemented in ``class_TimeManager``) are at the heart of the engine; they interact mostly with:

 - other time managers, for synchronisation

 - with actors (inheriting, directly or not, from ``class_Actor`` or its child classes, like ``class_BroadcastingActor``), in order to schedule them


Time managers are created by the deployment manager (``class_DeploymentManager``) and may interact with its computing host managers (``class_ComputingHostManager``).

The actors that time managers schedule are created by the load balancer (``class_LoadBalancer``), which does its best to even the load on the corresponding computing hosts.

Time managers also drive the data-exchanging distributed service (``class_DataExchanger``), and the performance tracker (``class_PerformanceTracker``) monitors them (among other agents).



Detailed Description of the Integration of New Actors to the Simulation
.......................................................................




Actor Start-up Procedure
........................

When models become increasingly complex, more often than not they cannot compute their behaviour and interact with other models *directly*, i.e. as soon as they have been synchronised with the time manager.

For instance, quite often models need some random variables to define their initial state. This is the case for example of low voltage meshes, which typically have to generate at random their supply points and their connectivity. As explained in the `Actual Management of Randomness`_ section, this cannot be done when the model is not synchronised yet with the time manager: reproducibility would not be ensured then.

Therefore the complete initialisation of such an actor cannot be achieved from its constructor only, and it needs an appropriate mechanism to determine at which point it is finally ready.

Moreover, as the start-up of an actor may itself depend on the start-up of other actors (ex: the low-voltage mesh needs to wait also for its associated stochastic deployment policy to be ready, before being able in turn to live its life), Sim-Diasca provides a general mechanism that allows any actor to:

 - wait for any number of other actors to be ready

 - perform then some model-specific operations

 - declare itself ready, immediately or not, and notify all actors (if any) that were themselves waiting for that actor

The graph of waiting actors will be correctly managed as long as it remains acyclic.

This automatic coordinated start-up is directly available when inheriting from the ``Actor`` class.



Non-Termination Of Ticks
........................

Some models can be incorrectly implemented. They may crash or never terminate, or fail to report they finished their tick.

The simulator will wait for them with no limit of time (as there is no a priori upper bound to the duration needed by a model to process its tick), but in batch mode a ``watchdog`` process is automatically triggered.

It will detect whenever the simulation is stalled and notify the user, telling her which are the guilty process(es), to help their debugging.

There could different reasons why an actor does not report its end of tick, notably:

	- its internal logic may be incorrectly implemented, resulting in that actor being unable to terminate properly (ex: infinite loop)

	- the lingering actor (actor A) might be actually waiting for the acknowledgement from another actor (actor B) to which that actor A sent an actor message this tick

In the latter case the guilty process is in fact actor B, not actor A.

Both cases should be easy to interpret, as the time manager will gently nudge the lingering actors, ask them what they are doing, and then output a complete diagnosis, both in the console and in the simulation traces::

  Simulation currently stalled at tick #3168318240271, waiting for following actor(s): [<0.50.0>,<0.57.0>].
  Current tick not ended yet because:
   + actor <0.50.0> is waiting for an acknowledgement from [<0.1.0>]
   + actor <0.57.0> is waiting for an acknowledgement from [<0.1.0>,<0.38.0>]


Now moreover the engine is able most of the time to also specify the name of the actors that are involved, for a better diagnosis.



Distributed Execution In Practise
.................................

For the scenario test case to be able to run a simulation on a set of computing nodes from the user node, that node must be able to trigger the appropriate Sim-Diasca daemon on each computing node.

To do so, a SSH connection is established and the appropriate daemon is run. The recommended set-up is to be able to run a password-less connection to the computing nodes. This involves the prior dispatching of a private key is these nodes, and the use of the corresponding public key by the user host.

See `Enabling The Distributed Mode Of Operation`_ for the corresponding technical procedure.



Model Development
.................

All generic mechanisms discussed here (actor subscription, synchronisation, reordering and acknowledgement of actor messages, removal, waiting to be ready, etc.) have been abstracted out and implemented in a built-in ``Actor`` class, to further ease the development of models.

They should therefore inherit from that class and, as a consequence, they just have to focus on their behavioural logic.



Of Times And Durations
......................


User Time Versus Simulation Time
________________________________

Regarding simulation timing, basically in **batch** mode the actual *user time* (i.e. wall-clock time) is fully ignored, and the simulation engine handles only timestamps expressed in *virtual time*, also known as *simulation time*. The objective there is to evaluate model instances as fast as possible, regardless of the wall-clock time.

In **interactive** mode, the engine still bases all its computations on virtual time, but it forces the virtual time to match the real time by slowing down the former as much as needed to keep it on par with the latter (possibly making use of a scale factor).

Therefore the engine mostly takes care of simulation time, regardless of any actual duration measured in user time (except for technical time-outs).


*Units* of Virtual Time Versus Simulation *Ticks*
_________________________________________________

Virtual time can be expressed according to various forms (ex: a full time and date), but the canonical one is the **tick**, a quantum of virtual time whose duration is set by the simulation user (see the ``tick_duration`` field of the ``simulation_settings`` record). For example the duration (in virtual time) of each tick can be set to 20ms to define a simulation running at 50Hz.

**Ticks** are absolute ticks (the number of ticks corresponding to the duration, initially evaluated in gregorian seconds, between year 0 and the specified date and time), ans as such are often larger integers.

For better clarity and performances, the simulation engine makes heavy use of **tick offsets**, which correspond to the number of ticks between the simulation initial date (by default a simulation starts on Saturday, January 1st, 2000 at midnight, in virtual time) and the specified timestamp. So ``#4000`` designates a tick offset of 4000 ticks.

Note that one can sometimes see expressions like ``this happened at tick #123``. The dash character (``#``) implies that this must be actually understood as a tick offset, not as an (absolute) tick.


Models should define all durations in terms of (non-tick) time units, as actual, plain durations (ex: 15 virtual seconds), rather than directly in ticks or tick offsets (like ``#143232``). Indeed these former durations are absolute, context-less, whereas the corresponding number of simulation ticks depends on the simulation frequency: one surely does not want to have to update all the timings used in all models as soon as the overall simulation frequency has been modified.

So the recommended approach for models (implemented in child classes of ``class_Actor``) is to define, first, durations in time units (ex: 15s), and then only to convert them, as soon as an actor is created (i.e. at simulation-time), into a corresponding number of ticks (ex: at 2Hz, 15s becomes 30 ticks) thanks to the ``class_Actor:convert_seconds_to_ticks/{2,3}`` helper functions [#]_.

.. [#] A corresponding method (``convertSecondsToTicks/2``) could be used instead, however this method has virtually no chance of being overloaded any day, so using the helper functions is not a problem.


This ``class_Actor:convert_seconds_to_ticks/2`` function converts a duration into a non-null integer number of ticks, therefore a rounding is performed, and the returned tick count is at least one (i.e. never null), in order to prevent that a future action ends up being planned for the current tick instead of being in the future, as then this action would never be triggered.

Otherwise, for example a model could specify a short duration that, if run with lower simulation frequencies, could be round off to zero. Then an actor could behave that way:

 - at tick #147: set action tick to current tick (147) + converted duration (0) thus to #147; declaring then its end of tick
 - next tick: #148, execute::

  case CurrentTick of

	ActionTick ->

		do_action();
		...

However ``CurrentTick`` would be 148 or higher, never matching ``ActionTick=147``, thus the corresponding action would never be triggered.


Ticks Versus Tick Offsets
_________________________

*Ticks* are absolute ticks (thus, generally, huge integers), whereas *tick offsets* are defined relatively to the absolute tick corresponding to the start of the simulation.

Of course both are in virtual time only (i.e. in simulation time).

Tick offsets are used as much as possible, for clarity and also to improve performances: unless the simulation runs for a long time or with an high frequency, tick offsets generally fit into a native integer of the computing host. If not, Erlang will transparently expand them into infinite integers, which however incur some overhead.

So, in the Sim-Diasca internals, everything is based on *tick offsets*, and:

 - when needing *absolute ticks*, the engine just adds to the target offset the initial tick of the simulation

 - when needing a *duration* in simulation time, the engine just converts tick offsets into (virtual, floating-point) seconds

 - when needing a *date* in simulation time, the engine just converts a number of seconds into a proper gregorian date



Starting Times
______________

By default when a simulation is run, it starts at a fixed initial date, in virtual time [#]_, i.e. Friday, January 1, 2010, at midnight. Of course this default date is generally to be set explicitly by the simulation case, for example thanks to the ``setInitialTick/2`` or ``setInitialSimulationDate/3`` methods. These timings are the one of the simulation as a whole.

.. [#] This arbitrary date was previously set to the current real time, so that the simulations started from the present time of the user. However we could then have variations in the results despite reproducible simulations, if using models depending on the absolute (virtual) date (ex: in the simulation, ``each 1st of April, do something``).

Simulations will always start at tick offset #0 (constant offset against a possibly user-defined absolute tick) and diasca 0.

On the actor side, each instance has its own ``starting_time`` attribute, which records at which global overall simulation tick it was synchronized to the simulation.




Implementing an Actor
---------------------

Each model must inherit, directly or not, from the actor class (``class_Actor``).
As a consequence, its constructor has to call the one of at least one mother class.

Each constructor should start by calling the constructors of each direct parent class, preferably in the order in which they were specified; a good practice is to place the model-specific code of the constructor after the call to these constructors (not before, not between them).


An actor determines its own scheduling by calling oneways [#]_ helper functions offered by the engine (they are defined in the ``class_Actor`` module):

 - ``addSpontaneousTick/2`` and ``addSpontaneousTicks/2``, to declare additional ticks at which this instance requires to develop a future spontaneous behaviour (at their diasca 0)

 - ``withdrawnSpontaneousTick/2`` and ``withdrawnSpontaneousTicks/2``, to withdraw ticks that were previously declared for a spontaneous behaviour but are not wanted anymore

 - ``declareTermination/1``, to trigger the termination of this actor


.. [#] Note that corresponding helper functions are also defined (ex: ``class_Actor:add_spontaneous_tick/2``); they can be called directly if the user is sure that he will never need to override their oneway counterpart.



An actor is to call these helper functions from its ``actSpontaneous/1`` oneway or any of its actor oneways. This includes its ``onFirstDiasca/2`` actor oneway, which is called as soon as this actor joined the simulation, so that it can define at start-up what it intends to do next, possibly directly at this very diasca (no need for example to wait for the first next tick).

Even if actors are evaluated in parallel, the code of each actor is purely sequential (as any other Erlang process). Hence writing a behaviour of an actor is usually quite straightforward, as it is mostly a matter of:

 - updating the internal state of that actor, based on the changes operated on the value of its attributes (which is an immediate operation)

 - sending actor message(s) to other actors (whose processing will happen at the next diasca)

On each tick the engine will automatically instantiate as many diascas as needed, based on the past sending of actor messages and on the management of the life cycle of the instances.

So the model writer should consider diascas to be opaque values that just represent the "happened before" relationship, to account for causality; thanks to these logical moments which occur during the same slot of simulated time, effects always happen strictly after their causes.

As a consequence, the model writer should not base a behaviour onto a specific diasca (ex: "at diasca 7, do this"); the model should send information to other instances or request updates from them (in both cases thanks to other messages) instead.

So, for example, if an actor asks another for a piece for information, it should just expect that, in a later diasca (or even tick, depending on the timeliness of the interaction), the target actor will send it a message back with this information.

The point is that if the model-level protocol implies that a target actor is expected to send back an answer, it *must* do so, but at any later, unspecified time; not necessarily exactly two diascas after the request was sent: we are in the context of asynchronous message passing.

This allows for example an actor to forward the request to another, to fetch information from other actors, or simply to wait the duration needed (in virtual time) to account for any modelled processing time for that request (ex: "travelling from A to B shall last for 17 minutes").


When actor decides it is to leave the simulation and be deleted, it has to ensure that:

 - it has withdrawn all the future spontaneous ticks it may have already declared

 - it calls its ``declareTermination/{1,2}`` oneway (or the ``class_Actor:declare_termination/{1,2}`` helper function)


The actor must ensure that no other actor will ever try to interact with it once it will have terminated, possibly using its deferred termination procedure to notify these actors that from now they should "forget" it.

Please refer to the ``Sim-Diasca Developer Guide`` for more information.


:raw-latex:`\pagebreak`


Latest Enhancements
-------------------

These evolutions have been implemented for the ``2.0.x`` versions of Sim-Diasca, starting from 2009.


Distributed Time Manager
........................

The ``Time Manager`` was described as a centralised actor, but actually, for increased performances, the time management service is fully distributed, thanks to a hierarchy of time managers.

By default there is exactly one time manager per computing host, federating in an Erlang node all cores of all its processors and evaluating all the actors that are local to this host (and only them).

One of these time managers is selected (by the deployment manager) as the root time manager, to be the one in charge of the control of the virtual time. The other time managers are then its direct children (so the height of the scheduling tree is by default equal to 1).

Other kinds of trees could be chosen: they might be unbalanced, have a different heights (ex: to account for multiple clusters/processors/cores), etc., as shown in the physical diagram below:

:raw-html:`<img src=SimDiasca-physical-dispatching-english.png></img>`
:raw-latex:`\includegraphics[scale=0.24]{SimDiasca-physical-dispatching-english.png}`


The overall objective is to better make use of the computing architecture and also to minimize the induced latency, which is of paramount importance for synchronous simulations (we want to be able to go through potentially short and numerous time-steps as fast as possible).



Two protocols are involved in terms of scheduling exchanges, as shown in the logical diagram:

 - one for higher-level synchronisation, between time managers

 - another for lower-level actor scheduling, between a local time manager and the actors it drives


:raw-html:`<img src=SimDiasca-logical-dispatching-english.png></img>`
:raw-latex:`\includegraphics[scale=0.32]{SimDiasca-logical-dispatching-english.png}`


.. comment To be updated:
  A corresponding sequence diagram can be the following:
  :raw-html:`<img src=SimDiasca-placement-scheduling-sequence.png></img>`
  :raw-latex:`\includegraphics[scale=0.27]{SimDiasca-placement-scheduling-sequence.png}`


Of course there will be many more actors than displayed on the diagram created on each computing node (typically dozens of thousands of them), therefore a lot of scheduling messages will be exchanged between these actors and their local time manager instance.

The point is that these (potentially numerous) messages will incur as little overhead as possible, since they will be exchanged inside the same computing node: only very few scheduling messages will have to cross the node boundaries, i.e. to be conveyed by the bandwidth-constrained network. We trade the number of messages (more numerous then) for their network cost, which is certainly a good operation.


The load balancer has to be an actor as well (the only special one, created at start-up), since, when the simulation is running, it must be able to enforce a consistent order in the actor creations, which, inside a time step, implies the use of the same message reordering as for other actors.

In the special case of the simulation set-up, during which the initial state of the target system is to be defined, the initial actors have to be created, *before* the simulation clock is started. Only one process (generally, directly the one corresponding to the simulation case being run; otherwise the one of a scenario) is expected to create these initial actors. Therefore there is no reordering issues here [#]_.

.. [#] However race conditions must be avoided there (between creations and also with the simulation start), this is why all initial creations are by design synchronous.




Advanced Scheduling
...................

Each model may have its own temporality (ex: a climate model should imply a reactivity a lot lower than the one of an ant), and the most reactive models somehow dictate the fundamental frequency of the simulation.

A synchronous simulation must then be able to rely on a fundamental frequency as high as needed for the most reactive models, yet designed so that the other models, sometimes based on time-scales many orders of magnitude larger, can be still efficiently evaluated; scalability-wise, scheduling all actors at all ticks is clearly not an option (short of wasting huge simulation resources).

Moreover most models could not simply accommodate a sub-frequency of their choice (ex: being run at 2 Hz where the fundamental frequency is 100 Hz): their behaviour is not even periodic, as in the general case it may be fully erratic (ex: determined from one scheduling to the next) or passive (only triggered by incoming actor messages).

So Sim-Diasca offers not only a full control on the scheduling of models, with the possibility of declaring or withdrawing ticks for their spontaneous behaviours, but also can evaluate model instances in a rather efficient way: this is done fully in parallel, only the relevant actors are scheduled, and jumps over any idle determined to be idle are automatically performed (based on a consensus established by the time managers onto the next virtual time of interest; if running in batch, non-interactive, mode).

With this approach, and despite the synchronous simulation context (i.e. the use of a fixed, constant time step), the constraints applied to models are very close to the ones associated to event-driven simulations: the difference between these two approaches is then blurred, and we have here the best of both worlds (expressiveness and performance).

Finally, models ought to rely as much as possible on durations (in virtual time) that are expressed in absolute units (ex: "I will wait for 2 hours and a half") instead of in a frequency-dependent way (ex: "I will wait for 147 ticks"): the conversion is done automatically at runtime by the engine (with a mechanism to handle acceptable thresholds in terms of relative errors due to the conversions), and the model specifications can be defined as independently as possible from the fundamental frequency chosen by the simulation case.




Zero-Time Bias Modelling
........................

Despite such a tick-level flexibility, by default time biases cannot be avoided whenever solving causality over ticks. Indeed, if, to ensure effects can only happen strictly after their causes, actor messages are evaluated on the tick immediately following the one of their sending, then for example all request/response exchange patterns will induce a two-tick latency.

This is unfortunate, as this latency is not wanted (not present in the models), and moreover depends on the fundamental frequency of the simulation. No immediate interaction can happen then, and if the target of a request needs to get information from other actors, the latency will still increase, with no upper limit.

To solve this significant modelling constraint, a zero-time bias feature has been added to Sim-Diasca (in 2012), introducing the notion of *diascas*, which are numbered logical moments inside a tick. A tick is then actually composed of an unbounded series of diascas, the first one (diasca 0) corresponding to the time when all actors having planned a spontaneous behaviour are to develop it. This may include the sending of actor messages, which in turn leads to the creation of the next diasca: each actor having received a message at a diasca will then be scheduled accordingly at the next diasca, and may then send messages, and so on.

As a consequence, the advanced scheduling, once enriched with diascas, is able to adapt to any model-level scheduling, and to support all interactions, either immediate or delayed, involving any communication chain underneath, in a causality-free way, and still in a massively parallel manner.

This flexibility regarding virtual time with no biases opens in turn new outlooks, for example to run in parallel, with Sim-Diasca, models that are written in the context of a mere sequential simulator, or, possibly, to go towards hybrid simulations, where some models are ruled by continuous equations yet are to be integrated with the discrete part of the simulation, helped by a numerical solver and further integration efforts.





How Virtual Time is Managed
===========================


Virtual Time Versus Real Time
-----------------------------

In batch mode, the time of the simulation (a.k.a. virtual time) is fully decorrelated from the user, wall-clock time: the engine will run as fast as possible, but will take as long as needed to fully evaluate simulated events. As a consequence, depending on the computer resources that are used, the resulting simulation might be faster or slower than real-time.

In interactive mode, provided that the hardware is able to run the simulation faster than the real time (possibly once a user-specified scaling factor has been applied), the engine will perform the necessary waiting so that the virtual time stays on par with the real time, allowing for possible third-party interactions with the simulation (actual devices, humans in the loop, etc.).


Quantification & Timings
------------------------

The virtual time is quantised, i.e. chunked into slices of equal durations (this is why Sim-Diasca is a discrete *synchronous* simulation engine). These periods are called ``simulation ticks`` (often shorten as ``ticks`` here).

For example, if the user specifies a tick duration of 20ms, then all durations will be multiples of 20ms (possibly 0ms - instant actions are supported) and the simulation will run at 50Hz.

Note that, for the models to be as much as possible uncoupled from the simulation frequency, they should express their timings in terms of actual "absolute" durations (ex: 100ms), rather than in terms of a number of ticks (ex: 5 ticks, at 50Hz).

This way, changing the simulation frequency (ex: setting it to 100Hz) will not imply that all models need to have their internal timing settings updated accordingly; indeed the engine is able to convert at runtime such actual durations into the relevant number of ticks, and will automatically ensure that the relative quantification error that is then induced stays below a threshold (either the default one or a user-defined one). If the quantification error is too high, the simulation will just report it and stop.


Spontaneous Behaviours & Triggered Actions
------------------------------------------

At its creation (whether initial or in the course of the simulation), each actor is scheduled once and is able to tell the engine at which tick (if any) it plans to develop its next spontaneous behaviour.

The engine (actually, the root time manager) is then able to know for each new tick whether it should be scheduled (this will happen iff at least one actor planned a spontaneous action for that tick). This knowledge will then allow the simulation to automatically jump over ticks that are known to be idle (i.e. with no actor to schedule). This feautre allows such a synchronous engine to provide features that are quite similar to the ones provided by asynchronous engines.

However, such a simulation would be useless if the actors could not interact with other actors: during a tick, any actor is able to trigger actions on any other actor.


Causality & Diasca
------------------

To ensure, even in a parallel and/or distributed setting, that no cause can happen after its effects, the model instances can only communicate through the exchange of *actor messages*.

These messages can be seen as method calls which are properly intercepted, reordered and actually triggered by the engine.

For that the notion of diasca is introduced: each tick is evaluated as a series of diascas (starting at diasca D=0), each of them being a logical moment inside that tick.

The purpose of diascas is to split each tick so that the consequence of an event (an actor message is sent at diasca D) happens at the next diasca (the actor message is processed by its recipient at diasca D+1).

These triggered actions may in turn result in new actor messages being sent, resulting in as many diascas as needed (D+2, D+3, etc.) being instantiated. The current tick will terminate only when no more diascas are requested, i.e. when there is no more immediate action declared. Then the next tick will be scheduled, and will start from diasca 0 again.

As such, diascas do not correspond to any specific duration within a tick (which is by design the finest observable simulated duration): their purpose is just to allow to determine that some events happen before others, i.e. to maintain causality.



In-Depth: Scheduling Implementation
===================================

The simulations being distributed, the time management is itself distributed.

For that, a hierarchy of time managers is defined. Any scheduling tree can be defined, knowing that by default there will be one time manager per computing host (thus federating all cores of all local processors), and that they will form a flat tree: the engine will select an appropriate host for the root time manager (which will the sole manager of virtual time), and all others will be direct children of it.

Each time manager will schedule all the actors that are local to its node. This way, the large majority of scheduling messages will remain local to an Erlang node (i.e. potentially millions), and only the inter-manager ones (i.e. a few) will be exchanged over the network.

At the beginning of a tick T that is to be scheduled (as idle ticks will be jumped over automatically), the root time manager will trigger a ``{ beginTimeManagerTick, T }`` to all its direct children, which will forward it recursively to their own children, if any. Then each time manager will send a ``{ beginTick, T }`` messages to all its local actors that were scheduled for a spontaneous action this tick.

They will develop then each (in parallel) their spontaneous behaviour, being implicitly at diasca D0=0 for that tick T. Each actor can then update its state and/or send inter-actor messages.

Sending an actor message in asynchronous (not blocking) and results in the recipient actor storing the corresponding method call for a deferred execution (at the next diasca), and to send a ``{ scheduleTrigger, T, D }`` message to its own local time manager (which might already have terminated its diasca) so that it knows that this actor will have then to be triggered. Once its manager acknowledged that message (needed to prevent race conditions) thanks to a ```trigger_planned`` message, the recipient actor can then directly acknowledge to the sending actor that its message was processed, thanks to a ``{ acknowledgeActorMessage, T, D, ... }`` message.

Once a scheduled actor will have completed its spontaneous actions and received all acknowledgements for the actor messages it sent this diasca, it will notify its time manager by sending it a ``{ spontaneous_behaviour_completed, T, D0, DiascaOutcome }`` message, where DiascaOutcome is ``no_diasca_requested`` if it did not send any actor message, or ``next_diasca_needed`` otherwise.

Once all actors scheduled by a time manager complete a given diasca, this manager will report to its direct parent manager (if any) whether or not a new diasca is needed, by sending it a ``{ childManagerSpontaneousActionsCompleted, ... }`` message specifying, among other information, either ``no_diasca_requested`` or ``next_diasca_needed``.

As soon as at least one actor among all the scheduled actors for this diasca sent at least one actor message, the root time manager is notified that a new diasca D (still for tick T) is needed. Time managers will then recursively receive a ``{ beginTimeManagerDiasca, T, D }`` message and in turn will send, to the actors they schedule that received during the previous diasca an actor message, a ``{ beginDiasca, T, D }`` message.

Each of these triggered actors will then reorder all the messages it received during the previous diasca, and then process them in turn, possibly changing its state and/or sending new actor messages, which will in turn lead to a new diasca being needed, etc.

Once such a triggered actor completed its diasca, it sends back to its time manager a ``{ diasca_completed, T, D }`` message. Once all the local actors triggered for this diasca did so, their time manager sends a ``{ child_manager_diasca_completed, ... }`` message specifying, among other information, either ``no_diasca_requested`` or ``next_diasca_needed`` to its own direct parent manager.
