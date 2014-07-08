:raw-latex:`\pagebreak`


.. _enhancements:

------------------------------
Sim-Diasca Future Enhancements
------------------------------


:raw-html:`<img src="xkcd-researcher_translation.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-researcher_translation.png}`


General Requirements
====================

These requirements are lacking or are only poorly supported with the Sim-Diasca 1.x version series, and are fulfilled with the Sim-Diasca 2.x version series.



Actor Creation
--------------

An actor can be created either from a simulation launcher (ex: a test case), most often before the simulation started, or by another actor, usually in the course of simulation.

In both cases, these creations should lead to a totally reproducible simulation, moreover with no regard for the technical context (like the sets of computers being used).

To do so, an intermediate agent is used, the load balancer, which is a simulation actor and therefore can rely on the mechanisms for reproducibility.

Therefore the only simulation agent that will effectively create (spawn) a new simulation actor will be the load balancer.


Load Balancing
--------------

The load balancer, which is a singleton, will create actors remotely, on one of the available nodes, based on its balancing policy.

A priori the load balancer does not strictly need to create actors synchronously (as it will serialize their AAI assignment), but it needs anyway to return to the caller a notification that the created actors are ready, so that the caller can continue its operations without race conditions (ex: for a creating actor, it means finishing its own tick). Otherwise one could not be sure for example that the created actor managed to properly subscribe to its time manager during the same tick.


As actors may have to be created either before or in the course of a simulation, there are two ways of telling the load balancer to spawn them:

 - before the simulation started, the scenario or test case will call its ``bootstrap_actor`` request (not a oneway so that the caller can know for sure that the creation is over and, for example, can start the simulation from a proper and reproducible initial situation)

 - in the course of the simulation: then the creator will be one of the current actor, and the creation will be based on an actor message sent to the load balancer, ``spawn_actor``


Some more advanced load-balancing policies are interesting to provide, for example: the more instances interact, the closer they should be placed, the ideal situation being to have them run on the same computing node; the mere possibility of providing a hint to the load-balancer helps already a lot.


Indeed, this is just a matter of passing to the load balancer the same extra parameter, any Erlang term (ex: an atom like ``house_111452``), to the load-balancer when creating all actors corresponding to a given home (here the #111452). If having N candidate computing nodes, in that case the load balancer would bypass its default placement policy and always choose the ``N1 = hash(house_111452) rem N`` node (i.e. the hash value of the specified placement hint, modulo the number of nodes), ensuring all devices of that home - expected to be tightly linked - will be placed on the same node, ``N1``. This could lead to a rather uniform distribution across nodes, while minimising very effectively the network traffic.


Actor Identifiers
-----------------

As in all cases - reproducible or ergodic - messages will be reordered based on various information including the identity of the sender, the simulation engine must rely on actor identifiers which are themselves reproducible.

The Sim-Diasca 0.1.x versions relied for that on Erlang process identifiers (PID), which where indeed reproducible in the context of a given computing architecture, but which would offer no guarantee should, for example, the number of computers change. Therefore in this case a given actor could be identified by different Pid, which in turn would lead to different reorderings, and therefore different simulation outcomes.

With the Sim-Diasca 0.1.x versions, actors use abstract identifiers, not technical ones like the Pid. In the course of a simulation, there is a one-to-one relationship (a bijection) between an *Abstract Actor Identifier* (AAI) and a Pid. But the same simulation run twice will exhibit actors bearing the same AAI with presumably different Pid.

AAI are managed by the load balancer, which simply maintains a counter of actors as it creates them, and keeps track of their Pid to be able to answer to look-ups.

The AAI of an actor is assigned when it is created, directly as the first construction parameter.

To avoid listing such an additional constructor parameter that we would have preferred invisible, the load balancer could have sent a message instead, that the actor would have waited in its constructor. However it is more complex, requires to send more networked messages, and finally might maybe be hidden with an appropriate parse transform.

The two-way resolving between AAI and Pid should probably be taken in charge preferably by a dedicated agent.



Actor Start-up
--------------

The fundamental frequency of the simulation should be automatically specified to any created actor, so that it can adjust its timings or report an unability to comply with the simulation frequency.

After the start-up phase, each actor should know at least:

 - the Pid of its time manager
 - the fundamental frequency of the simulation or, more precisely, the duration in simulation time of a elementary time step
 - its own random seeding for reproducibility and al
 - its Abstract Actor Identifier (AAI)
 - its scheduling policy



Actor Scheduling
----------------

Now the spontaneous and triggered behaviours of an actor do not have to happen at each tick: an actor can freely choose at which future simulation tick it should be scheduled next by the timer manager, whereas the actor messages sent to it will dictate when it will process them.

So an actor can behave spontaneously either as a periodical actor (being scheduled 1 tick every N), as a step-by-step actor (determining, while being scheduled, when it should be scheduled next), or as a purely passive actor (being only triggered by incoming actor messages, not having any spontaneous behaviour).

Actors will have also to be able to withdraw or change a previously selected tick. This can be useful when an actor receives an actor message between two spontaneous schedulings and based on that decides to change the planned one.

To do so, at the beginning of a tick when an actor is expected to develop some behaviour, it will be triggered by its time manager:

 - either by a ``spontaneous_top`` message, meaning this actor had planned to develop its spontaneous behaviour at this tick, and implicitly meaning that it has no actor message to process

 - or a ``triggered_top`` message, meaning at least there is at least one pending actor message to be processed, and implicitly meaning that it has no spontaneous behaviour to develop at this tick

 - or a ``twofold_top`` message, meaning there is at least one pending actor message to be processed *and* that this actor had planned to develop its spontaneous behaviour at this tick ; they will be managed in that order

With each of these top messages, the current simulation tick will be passed by the time manager.

Once the actor will have managed the ``top`` message it received, and once it will have successfully waited for the pending acknowledgement of any actor messages it sent this tick, it will notify its time manager its tick is finished by one of the following messages:

 - ``{done,N}`` where N is a (strictly positive) number of ticks before this actor should be next scheduled for a spontaneous behaviour; for example, if during the tick 100 an actor returned ``{done,2}``, then it will be scheduled for its spontaneous behaviour only at tick 102, possibly jumping over tick 101 if it did not receive any actor message at tick 100 ; having each actor specify explicitly its next spontaneous tick is the most flexible possible policy ; for example periodical schedulings or purely passive ones are just special cases

 - ``{done,none}`` if this actor intends to remain purely passive (i.e. only triggered by message, with no spontaneous behaviour), at least until the first next receiving of an actor message

 - ``terminating`` if this actor plans its removal at the next tick ; it will then receive a ``termination_top`` at the tick, and then nothing more


Although an actor may send directly these messages, they can be automatically handled by ``manage_end_of_tick``, depending on the scheduling policy declared by the actor, in ``passive``, ``{periodical,P}`` and ``custom``.

Each time manager will maintain an ordered list of the next ticks to schedule. If no event is planned in the simulated system for a period of virtual time, then the simulation will automatically jump directly over that period (i.e. no resource will be wasted examining idle ticks).

For reliability and testing purposes, the current tick of the actor can be appended to each of these messages, so that the time manager can check whether times are properly synchronised.

An actor can send at any time during its tick a ``{withdraw_spontaneous,Tick}`` message telling its time manager it does not want any more to be scheduled for spontaneous behaviour on the specified (absolute) tick.



Inter-Actor Communication
-------------------------

When an actor A1 needs to communicate an information to an actor A2, A1 will send an actor message to A2.

This will actually involve the sending of three messages:

 .# A1 sends the actual actor message to A2
 .# upon reception, A2 sends:

   - an acknowledgment message to A1, so that A1 can finish its tick
   - a ``schedule_trigger`` message to its time manager, so that this manager schedules it back on the next tick in order for this message to be processed by A2

If A2 already knows that it will be triggered next tick *in order to process actor messages* (i.e. regardless on any spontaneous scheduling), it may choose not to notify again its time manager.

We could have imagined that, instead of A2, A1 could have contacted the time manager so that A2 is triggered on the next tick. However, in a distributed context, A1 and A2 may depend on different time managers, and we want to notify the one of A2, which handles A2, not the one, potentially different, of A1.

This is why it is the task of A2 to send adequately the ``schedule_trigger`` message. Not only A2 knows which time manager to notify whereas A1 does not, but also it allows to use only one potentially non-local (networked) message instead of two.



Inter-Time Manager Synchronisation
----------------------------------

A time manager can have zero or one parent time manager (a time manager cannot be its own parent and no child manager should be set as a parent), any number of child time managers, and any number of actors to manage directly.

Therefore the time managers respect a hierarchical structure. As in each simulation any two time managers must be, directly or not, ancestor and heir (they must belong to the same graph), the structure is actually a tree, whose root corresponds to the time manager directly in touch with the user, and whose leaves are either time managers or, more probably, actors (an actor cannot be placed elsewhere than on a leaf).

When a tick is finished, all time managers, from bottom to top, reports the first next tick they have to schedule, and the next simulation tick will be the one that will happen sooner.

So each time manager will determine, based on its own actors (if any) and on its direct child time managers (if any), what is the next tick T it would schedule (the soonest of the reported next ticks), then sends to its parent time manager (if any) a ``{next_tick,T}`` message.

Then when these messages reach the overall time manager (the root one, the only one having no parent time manager), the smaller tick of all is known, the consensus is found and sent down recursively in the scheduling tree of time managers with a ``{begin_tick,T}`` message. Each time manager will in turn translate it with the proper ``top`` messages for the actors they drive.



Granularity of Synchronisation
------------------------------

The scheduling tree can be of any depth, and we could imagine having one time-manager per core, per processor, per computer, or per simulation.

The trade-off we currently prefer is to let the Erlang SMP interpreter spread as much as possible in a computing node, i.e. across processors and cores.

For example, with a computer relying on two processors with four cores each, we could have imagined 8 time managers (one per core), or 2 (one per processor), however just having one of them is possible and probably better, performance-wise. So we would have here one Erlang node making use of eight run queues. The optimal number of such queues might be further optimised.



Reproducibility and Ergodicity
------------------------------

The simulation user can request the engine to work according to one of the following schemes:

 - ``reproducible``, with or without a user-specified random seed
 - ``ergodic``

In reproducible mode, running twice the same simulation (same scenario, with possibly different computing contexts) should output exactly the same results.

In ergodic mode, each simulation execution will follow a specific possible trajectory of the system, knowing that statistically, over a large number of executions, the exploration of the possible states should be fair, i.e. all possible situations allowed by the models should be able to show up, and moreover they should occur with respect to their theoretical probabilities, as dictated by the models.

In practical, the reproducible mode without a user-specified random seed will just result in each actor reordering its messages according to their hash.

Thus the actor will be seeded (for any need in terms of generation of stochastic values they could have) but will not perform any additional message permutations [#]_. A default seed will then be used.

.. [#] Therefore this mode should be slightly faster than the others.


On the contrary, the other modes will rely on the actor-specific seed to perform an additional permutation of the messages.

More precisely, that seed will be the user-specified one if reproducible, or a seed automatically determined from current time if ergodic.

As the seed used in an ergodic context is recorded in the simulation traces, any ergodic execution can be later run again at will by the user, simply by specifying that seed in a new simulation execution, this time in reproducible mode.

As a consequence of these settings, in the context of a simulation the time managers, which are created by the load balancer, will:

 - all be given a seed, and will generate a specific seed for each actor they manage
 - request their actors either to reorder their messages based on hash only, or with an additional permutation



Reordering Of Actor Messages
----------------------------

Depending on the simulator settings, the reordering of the actor messages received for a given tick will be performed either so that reproductivity is ensured (i.e. messages are sorted according to a constant arbitrary order, the default one or one depending on a user-defined seed), or so that "ergodicity" is ensured, i.e. so that all possible reordering of events (messages) have a uniform probability of showing up.

In all cases a basic constant arbitrary order is obtained, based on ``keysort``, which sorts the actor messages according to the natural order defined over Erlang terms [#]_.

.. [#] Therefore this reordering does not involve computing the hash value of terms.

Let's suppose for example that an actor has, for the current tick, the following message list, whose elements are triplets like ``{SenderActorPid,SenderActorAai,ActorMessage}``::

  L = [{pa,5,5},
	   {pb,4,5},
	   {pc,6,5},
	   {pd,1,7},
	   {pe,10,5},
	   {pf,2,5},
	   {pg,3,5},
	   {ph,7,5},
	   {ph,7,1},
	   {pa,5,8},
	   {pa,5,1}]


The constant arbitrary order is obtained thanks to ``lists:keysort( 3, lists:keysort(2,L) )`` [#]_.

.. [#] One can see that this ordering does not depend on the PID of the sending actors (which is the first element of the triplet), as, for a given simulation,  these technical identifiers may vary depending on the computing hosts involved, whereas we want a stable reproducible order, independent from any technical context.

This means we sort first on the AAI (which is likely to be quite quick), like in::

  lists:keysort(2,L).
  [{pd,1,7},
   {pf,2,5},
   {pg,3,5},
   {pb,4,5},
   {pa,5,5},
   {pa,5,8},
   {pa,5,1},
   {pc,6,5},
   {ph,7,5},
   {ph,7,1},
   {pe,10,5}]


Once the entries are sorted in increasing AAI order (element #2), knowing that an actor may have sent multiple messages to that same actor, then we sort these entries based on their messages [#]_ (element #3)::

  lists:keysort( 3, lists:keysort(2,L) ).
  [{pa,5,1},
   {ph,7,1},
   {pf,2,5},
   {pg,3,5},
   {pb,4,5},
   {pa,5,5},
   {pc,6,5},
   {ph,7,5},
   {pe,10,5},
   {pd,1,7},
   {pa,5,8}]

.. [#] Knowing that two different actors may send the same exact message to a given actor (ex: ``{setColor,red}``).

So, at the end, the reordering ensured that messages are always sorted by increasing AAI and, when multiple messages share the same AAI (i.e. they were sent by the same actor), these messages are always sorted identically (i.e. according to an increasing message order).

At this point a basic reproducible order, totally independent from the technical context, is ensured.

Then, depending on whether reproducibility or ergodicity are targeted, further reorderings are performed over that constant base.

If the user selected reproducibility, the list of actor messages obtained from the basic reordering are then uniformly permuted, according to the simulation seed, which is either the default one or a user-defined one.

If the user selected ergodicity, a fair exploration of all possible simulation outcomes is obtained by operating exactly like for the reproducible case, except that the random seed is not user-specified, it is itself automatically drawn at random, based on user time.

Then each simulation will explore its own way one of the possible trajectories of the system, knowing that any of these trajectories is fully determined by the drawn ergodic seed.

As a consequence, whenever such an ergodic trajectory is deemed interesting, it can be replayed at will simply by feeding the simulator with the same seed, this time in the context of a reproducible execution based on that user-defined seed.



Simulation Deployment
---------------------

From the simulation scenario or from the test case, the load balancer must be created with the relevant simulation settings, including the list of candidate computing nodes.

The load balancer will then select the eligible computing nodes, which are the subset in the candidates nodes that can be connected:

 - the corresponding host must be up and running
 - it must be available from the network (ping)
 - a properly configured and named Erlang VM either can be launched on that node (with a password-less SSH connection) or is already launched
 - a two-way connection must be established with it (ex: the security cookie must match)



Performances
------------

One major goal of the Sim-Diasca 2.x versions is to increase the performances in a distributed context.

However some less demanding simulations will still be run in a local (non-distributed) context. So another requirement is to ensure that the new distributed mode of operation does not result in a loss of performances in a local context.




Load Balancing
==============

As discussed previously, in a distributed context, it is always possible for the user to specify on which machine each actor should be created and run.

This rather tedious process can be managed automatically and more efficiently by a ``load balancer``, i.e. a module that determines by itself an appropriate location for each new actor, and creates this actor accordingly.


Example of Use
--------------

An example of such interaction could be::

	% Here instances are created on each calculator in turn:
	BalancerPid = class_LoadBalancer:new_link( round_robin,
		[ host_a, host_b, host_b ] ),

	% The load balancer creates on each calculator as many local time
	% managers as there are available nodes.

	% Replaces class_PLCNetwork:remote_new_link(MyHost,35,4,rural):
	BalancerPid ! {instanciate_link,[class_PLCNetwork,[35,4,rural]],self()},

	PLCNetworkPid = receive

		{wooper_result,{instanciated,Pid,_Computer}} ->
			Pid

	end,
	[..]



Load Balancing Approaches
-------------------------

Instead of an hardcoded placement, a load balancer can perform:

 - either a ``static`` balancing, i.e. actors will be created regardless of the actual machine loads, with *a priori* rules (ex: round-robin)

 - or a ``dynamic`` one, i.e. thanks to heuristics the load balancer will try to dispatch the induced load as evenly as possible among the computing nodes, based on the measurement of their actual load over time


In both cases, using a load balancer will lead in most cases to break the reproducibility of the association between a given actor instance and a Pid: a static balancing over a varying number of computing nodes or a dynamic balancing in all contexts will result in a given actor to bear different Pid from a simulation to another [#]_.

.. [#] Not to mention a future possibility of actor migration.


As explained below, this is not what we want, as we aim to uncouple totally the results of the simulations from the technical environments that support them.

On a side note, once the user code is able to rely on a load balancer, it will not depend on any particular type of load balancer, since all balancers will all be given creation requests and will all return the Pid of the corresponding created instances.

Therefore one can start with a very basic load balancer (like a round-robin based one), knowing that the integration of a more advanced ones (say, a dynamic one using advanced heuristics) should not imply any model to be modified.

Another interesting feature would be to have a load balancer which would take into account the tightness of the coupling between a set of actors. Then, the more actors would interact, the stronger the tendency to instanciate them on the same node would be.

If such a guessing about coupling intensity seems difficult to achieve for a load balancer, the simulation user could hint it, for example by designating a group by an atom and specifying that atom at each creation of one of its member. Then the load balancer would just have to try to place all actors bearing that atom on the same node, whatever it is.


Actor Creation
--------------

In the course of the simulation, an actor may need to create another actor [#]_. In this case it has to request the creation to the load balancer.

.. [#] Otherwise an actor is *initial*, i.e. created by the simulation case before the simulation starts, see in this case the ``class_LoadBalancer:createActor/3`` request.

In the future, we could imagine following enhancements:

 - the creating actor could be able to specify a **placement hint**, which could be any Erlang term (generally, an atom), to increase the probability that coupled actors are created on the same node; so, for example, an anthouse A, itself created with a placement hint ``anthouse-a``, could specify the same hint whenever requesting the creation of an ant. Then the load balancer would compute the hash value of that hint and select always the same node based on that, provide this does not lead to a too unbalanced dispatching of actors onto nodes

 - the creating actor could be able to specify a **request identifier**, which would help it tracking which actors were created by the load balancer on its behalf; indeed, if an actor requests at the same tick the creation of an instance of two different classes, then by default, when it will be notified by the load balancer of these creations at the next tick, it will not be able to tell which returned PID corresponds to which instance, knowing that the load balancer had all its requests reordered






Reproducible Actor Identifiers
==============================

When running on reproducible mode, the arbitrary order enforced on concurrent messages received by a given actor at any given tick can be based on the actual message content, thanks to a hashing function, but in order to resolve the hash collisions we have to take into account the message sender as well.

Otherwise, when an actor A would be interacting with two instances B1 and B2 of a same class, B1 and B2 could quite possibly send the same message to A at the same tick (ex: ``{setColor,red}``). Then the content of the messages would be identical, their hash too, and the simulator would not be able to decide on their ordering.

Thus we need to rely on the sender information to perform a proper sorting of messages, but, unfortunately, if using a load balancer or if not using it but having to run on a changing computing infrastructure, Pid will not be suitable for that, short of being themselves reproducible.

Finally we need an actor identifier that is totally independent from the technical realm.

The solution will be implemented based on the load balancer.


To maintain a proper management of simulation time, all actors should be created:

 - either directly from the simulation case *and* synchronously (to prevent race conditions at start-up), before the simulation is run (i.e. before the time manager makes the simulation clock progress)

 - or during the simulation itself, but in this case a new actor must be created by an actor already synchronised

Otherwise the creation of new actors would not be synchronised with the simulation time (i.e. a given actor could be created, from a simulation to another, at different ticks) or if two actors were creating, each, another actor at the same tick, there would be a race condition.


When needing to rely on (unique) reproducible identifiers [#]_, to the best of our knowledge the only solution is to delegate the setting of identifiers to a centralised actor: no distributed algorithm can find a consensus on the new identifier to generate more easily than a counter-based centralised one.

.. [#] Actually we only need reproducible *orderings* of identifiers, but this weaker need could not be fulfilled with other solutions than actually reproducible identifiers (which is a stronger form).





Code Deployment
===============

When running a simulation on a set of computing nodes, on each of them the following software will be needed at runtime:

	- an Erlang interpreter
	- a set of BEAM files corresponding to:

		- the simulation engine (Sim-Diasca)
		- the simulation-specific models that run on top of it

The determining and gathering of these BEAM files is based in the buit-in installation procedure, with creates a proper, quite standard, installation base.

The Erlang interpreter *could* be deployed at runtime (a prebuilt version could be installed, at the expense of presumably light efforts), but it might be seen as a prerequisite, expected to be already available, instead.

In this case a few shell scripts could:

	- login (with SSH password-less authentication) on each computing node

	- launch the ``epmd`` daemon (*Erlang Port Mapper Daemon*) and an Erlang deployment client that would retrieve directly from a repository (possibly from the computer of the simulation user) all the relevant precompiled BEAM files

Then the simulation could be created automatically on a user-defined set of nodes and run transparently on them.



Performance Tuning
==================

Many actions could - and will - be taken to further enhance the performances of Sim-Diasca, including:

 - testing native compilation
 - integrating the "zero-overhead" WOOPER 2.0 version, based on parse transforms
 - using multiple 4GB VMs per host, to switch to a more compact 32-bit addressing; or making use of the "half-word emulator"
 - testing for concurrency errors, and tuning the application protocol to reduce overall latency
 - porting the simulation engine onto vastly concurrent resources (from IBM Bluegene/Q supercomputer to manycore cards like `Kalray <http://www.kalray.eu/>`_ or `Tilera <http://www.tilera.com/>`_)

We will ensure first that developing each of these enhancements is worth the time:

:raw-html:`<img src="xkcd-is_it_worth_the_time.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-is_it_worth_the_time.png}`


		   
Upstream Works
==============

There is a number of more advanced topics that we hope to tackle in the next months and years.

:raw-html:`<img src="xkcd-einstein.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-einstein.png}`

Among them, there is:

 - up to what point meta-programming can help further enhance the engine?
 - could there be a more high-level modelling language that could ease the work of domain experts (ex: UML-based graphical editors helping them to define models as if they were sequential) while still being automatically mappable to a massively concurrent simulation engine like Sim-Diasca?   
- could hybrid simulations (i.e. simulations that have elements both in discrete time and in continuous time) be supported by Sim-Diasca ? A first step would be to support the continuous-time paradigm alongside the discrete one, before trying to merge them; for example, energy-related systems may have to be simulated partly with differential equations that cannot be easily solved nor discretised, partly with more event-based behaviours, and of course both themes would likely need to be coupled for more integrated simulations

 

Miscellaneous
=============

 - improvement of random generator: use of the ``crypto`` module or other good-quality random source (ex: Linux entropy pool) and a pseudo-random number generator (ex: a Fast Mersenne Twister)

 - use an enhanced version of WOOPER tailored for speed and low memory footprint (based on parse transforms)

 - deploy distributed nodes and agents fully in parallel, with a per-host or per-node manager, rather than sequentially

 - switch to the use of several 32-bit VMs per host, to further increase the scalability

 - support IPv6 settings (currently: IPv4-only); should not be too complex   

   
