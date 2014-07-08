:raw-latex:`\pagebreak`

--------------------------
Sim-Diasca Modelling Guide
--------------------------


Objective & Context
===================

The goal here is to help bridging the gap between the modelling world and the simulation world: in this section we discuss what is the "language" that the simulation engine natively understands, so that a modelling team is able to prepare descriptions of models that can be almost readily implemented into actual (runnable) simulation models.



Basics Of Simulation Operation Mode
===================================


Actors & Models
---------------


Actor
.....

A simulation actor is the basic building block of a simulation. It is an autonomous agent, usually corresponding to an element of the simulated system, which, during the course of the simulation, will be automatically and appropriately scheduled.

Being scheduled means being given the opportunity to act, either spontaneously and/or after having been triggered by another actor.

Each actor has a private state, which is made of a set of any number of attributes.

An attribute has a name and a value, which can be of any data type, usually made of an arbitrarily-complex combination of primitive types (integer, logical text constant [#]_, floating-point value, etc.) and structures (fixed-size array [#]_, list, tree, associative table, etc.).

.. [#] Named ``atom`` in Erlang.

.. [#] Named ``tuple`` in most declarative languages, including Erlang.


For an actor, acting corresponds mostly to:

 - changing its private state (ex: changing the value of an attribute)
 - sending messages to other actors (ex: to notify them that some event occurred)
 - possibly, triggering side-effects (ex: sending a message to the distributed trace system)


.. Note:: Please refer to the Sim-Diasca Developer Guide for more implementation-level information about actors.


Model
.....

A model corresponds to the description of a type of simulation actors, regarding state and behaviour.

Technically a model is actually a specific class, whose instances are the simulation actors that respect that model.



Expressivity
............


:raw-html:`<img src="xkcd-candy_button_paper.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-candy_button_paper.png}`


Of course, Turing machines can be simulated, but more generally very few restrictions apply to models, which can be arbitrarily complex.

The most difficult issue to deal with comes probably from the latency (in virtual time) which is induced by the simulation mechanisms: an information (i.e. an actor message) sent at tick T cannot be processed by its recipient sooner than tick T+1.

This one-tick latency can be alleviated by increasing the simulation frequency. It will then just be a matter of choosing the preferred trade-off between latency and wall-clock simulation duration.





Time-Stepped Simulation
-----------------------

Models and actors have to comply with the operating conventions of the Sim-Diasca simulations.

In that context, the main convention is that these simulations are *time-stepped*. This means that the simulation time is sliced into chunks of constant durations.

Therefore we can define at which frequency a simulation should run: a simulation scheduled at 50Hz will rely on a fundamental period of ``1/50 = 20 ms``, thus all simulation time-steps (also known as *ticks*) will correspond to 20 ms of virtual time.



Uncoupling From Virtual Time
----------------------------

Actors are only aware of the passing of this simulation virtual time: the actual time that we, users, experience must not matter to them, otherwise the simulation would loose most of its expected properties.

For example, we do not want the same simulation to output different results depending on the processing power that happens to be available for it.

Thus, for an actor, whether the computing of a given virtual 20ms time-step actually lasts for 1 ms or for two minutes of our time shall be irrelevant; each of these 20 ms time-steps will last in our time exactly as long as needed to compute it appropriately, and the processing of two successive time-steps might require vastly different actual durations, in user time.



Formalisation
-------------

Modelling allows to bridge the gap between our knowledge of the system and its implementation in the context of a simulation. Generally, increasingly detailed models are specified: first as full text, then with more formal languages.

One can certainly use flowcharts:

:raw-html:`<img src="xkcd-flow_charts.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-flow_charts.png}`

But one may preferably rely on a few UML diagrams, including:

 - use case diagram
 - activity diagram
 - class diagram
 - sequence diagram

Other interesting diagrams might be:

 - communication diagram
 - state machine diagram

This is one of the most delicate steps, as often the domain experts are not able to write by their own their corresponding models: they generally make use of domain-specific languages, which are tailored for their needs but quite often are, simulation-wise, not standard.

So a translation step to the simulation language must generally take place, and usually domain experts cannot perform that work [#]_.

The best practice we recommend is to adopt a unified language to specify all models first, and to practise pair-work (one domain expert sitting on the side of a computer scientist/model developer) to ensure that the translation is correct. Indeed, mistakes can easily be made:

:raw-html:`<img src="xkcd-circumference_formula.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-circumference_formula.png}`



.. [#] These restrictions surely apply to, say, lower-level C++-based simulation engines which demand that models are written that way; and, more generally, models whose behaviour is intrinsically to be described algorithmically need anyway a programming language to be specified. However some of these languages are more suitable than others. To take a real-life example, for the `CLEVER <http://www.cleveronline.org/>`_ project, 5 days of Erlang training, 5 days of basic Sim-Diasca training and 3 days of advanced Sim-Diasca training have been sufficient so that a team with no prior knowledge about these topics was able to write not only models but even a whole specialisation layer of Sim-Diasca for the simulation of metering systems.


Actor Messages
--------------

Actors can communicate only thanks to the passing of specific messages, named *actor messages*.

During one tick, any actor can send any number of actor messages to any number of actors.

When an actor A sends during tick ``N`` an actor message to actor B, B will process it only during the next tick, ``N+1`` [#]_.


.. [#] This 1-tick latency is induced by the time-stepped nature of the simulation. This is a constraint indeed, but it can be alleviated (for example by anticipating exchanges and/or choosing a higher fundamental frequency for the simulation) and it is at the root of all the useful properties these simulations can rely on.

A corollary is then that if A requests an information from B during tick ``N``, A will process that information during tick ``N+2``.

Moreover during a tick an actor may receive multiple messages from multiple actors. No assumption should be made on their processing order within that tick, as the simulation algorithm will have reordered them to ensure the respect of the simulation properties (ex: reproducibility or ergodicity).




Actor Life-Cycle
----------------


Actor Creation
..............

An actor must be either created:

 - by the simulation scenario, before the simulation is started
 - by another (already synchronised) actor

In the latter case, if actor A requires the creation of an actor B during tick ``T`` diasca ``D`` (hence at ``{T,D}``), then B will be actually created at tick ``{T,D+1}``. On the next diasca (``{T,D+2}``) B will be scheduled for the first time (thanks to a call to its ``onFirstDiasca/2`` actor oneway), while A will be notified of this creation (and of the PID of B). 

Usually some specific actors, not directly corresponding to an element of the target system, are defined to create other actors.

For example a deployment policy for an information system can be such an actor, that will create devices according to a given statistical law, in the course of the simulation.

The simulation scenario itself can be modelled as one of these creating actors.


The actual creation of an actor in the course of the simulation is made of a few steps:

 - at ``{T,D}`` the creating actor issues a creation request [#]_ to the load balancer

 - at ``{T,D+1}``:

   - the load balancer processes that request, and creates synchronously the corresponding instance on the computing node it deems the most appropriate

   - during its construction the instance retrieves the overall scheduling settings from the time management agent it is in contact with, and as a consequence notifies it of how it intends to be scheduled; as the created actor is not synchronised yet to the simulation, its initial construction stage has to respect some restrictions; notably the actor is not able yet to interact with other actors or to consume stochastic variables yet

   - the load balancer sends back to the creating actor an actor message carrying the PID of the created instance, whose basic construction is finished (its constructor ended), but which is not ready to enter the simulation yet

 - at ``{T,D+2}``:

   - the creating actor processes the notification of the created instance, which includes its PID

   - the created actor is notified that the simulation has started for it (it is necessary as it could have been created before the simulation was started) and is scheduled for the first time; it is up to its model to determine whether this actor is ready to develop its behaviour immediately, as it may not have achieved its full initialisation yet (ex: it may be waiting for other actors to be themselves ready, and/or it might need to set some stochastic values to complete its initialisation, etc.)

   - as soon as the created actor deems it is itself ready (maybe from its first scheduled tick, maybe on later ticks), automatically any related actions will be triggered, and any actors waiting for that actor will be notified that it is ready now; then on the next tick the actor will be free to develop its normal behaviour; by default during its first scheduled tick an actor will not wait for any other actor, and therefore will call directly its (possibly overridden) ``onReady`` method; it will then be ready to develop its actual behaviour only on next tick (``N+3``)


.. [#] To preserve the simulation properties, the load balancer is itself a simulation actor and therefore the creation request is an actor message.



Actor Deletion
..............

An actor can decide to be removed from the simulation, usually before being deleted.

The removal process from the time manager will then be automatically conducted, but it is the responsibility of the the removed actor to ensure that no other actor will try to interact with it any more.

Usually the underlying logic ensures that it will be so, or the actor to remove notifies relevant actors of its ongoing removal.



Scheduling Sequence
-------------------

At each fundamental simulation tick, each actor may or may not be triggered by the time manager.

If an actor message was sent to it during the last tick, then the actor will automatically process that message, regardless of its scheduling policy.

Then, the time manager determines whether the scheduling policy of this actor implies that it should develop its spontaneous behaviour during this tick.

If yes, the actor will be notified of the current tick and be requested to act according to its planned behaviour.

If no, the actor will not be specifically contacted?



.. comment In practice, each scheduled simulation actor is notified that a new simulation tick began by a ``top`` message, sent by the time manager which is in charge of that actor.


.. comment The actor will then process any actor message it received during the last tick, before being free to develop any spontaneous behaviour it may have during that tick.


Stochastic Variables
--------------------

An actor may rely on any number of stochastic variables, each of which following any `probability density function <http://en.wikipedia.org/wiki/Probability_density_function>`_.

Sim-Diasca provides three of the most usual stochastic laws: uniform, Gaussian and exponential. User-specified laws can be added quite easily.

Once synchronised, an actor can draw any number of stochastic variables for any law during one tick, immediately (i.e. with a zero-tick latency).




:raw-latex:`\pagebreak`


Main Choices In Terms Of Actor Modelling
========================================


Fundamental Frequency
---------------------

As discussed previously, the root time manager in charge of a simulation will maintain its virtual time based on the fundamental overall frequency the simulation user specified: this frequency (ex: 50 Hz) directly dictates the duration in virtual time between two successive engine ticks (ex: 20 ms).

Therefore once this root time manager will have determined that all the actors to be scheduled this tick (the ones having to process actor message(s) and/or having to develop their behaviour on that tick) have reported that they have finished taking that current tick into account, it will then just increment the simulation tick, since the corresponding virtual 20 ms will have elapsed, and then declare that a new tick just has just begun.

Relying on a fundamental simulation frequency does not imply however that each and every simulation actor will have to be scheduled according to that exact overall frequency, i.e. at each tick. Each model is able to pick a scheduling policy that match best its needs.

Once all models have been established, the overall frequency of the simulation can be determined: it should be chosen at least equal to the highest frequency of the models involved.



Policies in Terms of Actor Scheduling
-------------------------------------

Generally the fundamental frequency will have been chosen so that the most reactive actors can be scheduled at the exact pace they require, but usually there will be also many other actors whose behaviour does not need to be evaluated as frequently.

Therefore, to ease the implementation of models and to preserve performances, the Sim-Diasca simulation engine allows models to request a scheduling more flexible than "every actor is triggered at each simulation tick".


Scheduling-wise, the three most common types of actors are:

  - *periodical actors*: an actor requesting a scheduling period of N would be triggered by the time manager one time step every N elapsed; therefore an actor could run, in virtual time, at a frequency of 10 Hz even if the fundamental frequency of the simulation was set for example to 50Hz

  - *step-by-step actors*: when such actors finish a time step, they may specify the next tick at which they should be triggered again (*look-ahead*), unless they receive an actor message in-between, in which case they may withdraw their already planned activation and set a new one, earlier or later

  - *purely passive actors*: these actors have no spontaneous behaviour, they are triggered only when they receive a message from another actor during the previous tick


These scheduling policies - and many others - can be implemented with Sim-Diasca thanks to the definition of future actions: each actor, during its triggered and spontaneous behaviours, is able to specify, if needed, a future tick at which it should be scheduled for a spontaneous behaviour.

Thus periodical actors will just define at the end of their spontaneous behaviour one future action which is to take place a fixed duration (in simulation time) after the current tick, step-by-step actors will define arbitrary future actions, and passive actors will never define any specific future action.




Frequency-Independent Timings
-----------------------------

In the context of a model, durations (in virtual time) are encouraged to be defined explicitly, absolutely, rather than directly as a given number of simulation ticks, so that models remain as much as possible independent from the actual frequency a simulation is running at.

For example, when a simulated device starts a new task and thus has to determine when a priori it will have finished the corresponding work and be available again, its model may evaluate the corresponding duration to "1400 ms" (in virtual time) rather than directly to an hard-coded "28 fundamental ticks".

Then only (i.e. at run-time), that duration, depending on the actual settings of the current simulation, will be converted to the appropriate number of ticks, so that a change in the simulation fundamental frequency will not impact models.

.. Note:: Using such absolute durations is not always straightforward, as being based on a fundamental frequency leads to a quantisation of durations: if for example a simulated device is scheduled by the voltage of the main supply (say, 50Hz), and if the simulation does not run at a multiple of 50Hz, then either the model will have to ignore the errors resulting from the approximated scheduling, or be designed - if possible - to accommodate to an arbitrary scheduling frequency, in spite of the issues this implies (like the accumulation of rounding or sampling errors).






Modelling Process
=================

.. comment .. Note:: This section is mostly a place holder.

Here are the questions that should be addressed simulation-wise, when writing a model.


Nature Of The Model
-------------------

Should all concepts to be ultimately simulated be represented by models of their own? Sometimes using a simple data structure owned by another actor is the most appropriate approach.

If a concept:

 - is used in multiple different contexts
 - and/or is used by multiple actors
 - and/or has a complex state and/or behaviour
 - and/or is not tightly coupled to any model

then most probably this concept should be mapped to a specific model, i.e. a dedicated class inheriting from ``class_Actor``.


Model Temporality And Reactivity
--------------------------------

Supposing we determined that the model was to be implemented as a class, we must then establish whether this model is able to perform spontaneous actions.

If yes (i.e. its instances are able to trigger actions not directly related to the receiving of a message received from other actors), then this is an active actor that will have a spontaneous behaviour, possibly periodical or erratic (step-by-step), etc.

The model is then able to specify with a total freedom its spontaneous scheduling, using notably ``addSpontaneousTick/2``, ``addSpontaneousTicks/2``, ``withdrawnSpontaneousTick/2`` and ``withdrawnSpontaneousTicks/2``, from its ``actSpontaneous/1`` oneway or any of ithe actor oneways it defined.

Please refer to the ``Sim-Diasca Developer Guide`` for further details.



State And Behaviour Of The Model
--------------------------------

These are very model-specific, but general rules still apply.

Processing an actor message and acting spontaneously both boil down to writing an appropriate method, which may send actor messages and/or return any updated state and/or trigger the removal of that actor.


Triggered Behaviour: Receiving of an Actor Message
..................................................

For example if an actor A needs to set the color of an actor B, then it may send to it an actor message specifying ``{setColor,red}``. Then, B will process it at the next diasca: its ``setColor`` method (actor oneway) will be automatically called and, based on the transmitted parameter, B will be able to update its state, for example by setting its ``color`` attribute to ``red``.

Should B receive an actor message requesting an answer (ex: ``getColor``), it would do so by sending back another actor message to A, like ``{notifyColor,red}``.


Spontaneous Behaviour
.....................

It is simply implemented by the calling of the ``actSpontaneous`` oneway method of that actor.

See also the `Sim-Diasca Implementation Review`_ section for a light example of modelling and implementation.
