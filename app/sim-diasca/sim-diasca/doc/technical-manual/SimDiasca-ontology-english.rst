:raw-latex:`\pagebreak`

---------------------------------
Let's Start With A Short Ontology
---------------------------------


What is an Ontology?
====================

As defined `here <http://en.wikipedia.org/wiki/Ontology_%28information_science%29>`_, *an ontology formally represents knowledge as a set of concepts within a domain, and the relationships among those concepts. It can be used to reason about the entities within that domain and may be used to describe the domain.*

We aim here to express an ontology about discrete simulations, so that we can define relevant terms and share them once for all.



Simulation Ontology
===================

The ontology is currently in a simpler form, the one of a glossary (terms, sorted alphabetically, and their definition).

The examples illustrating the definitions are taken from a hypothetical simulation case involving preys and predators (a typical use-case in this simulation domain).


Actor
		An ``actor`` is an instance of a ``model``.

		For example, "*This actor corresponds to this specific prey that we wanted to introduce in previous simulations, the one that is bound to be eaten first due to its starting location.*"



Model
		A ``model`` is a simplification of an element of the ``target system``. It is a type, from which instances (``actors``) can be created. It defines notably their state and behaviour.  

		For example, "*I am quite happy of the current predator model, it behaves nicely compared to what experience tells us. See how the actors created from it move?*"



Probe
		A ``probe`` is a producer of ``simulation results``, based on information made available by a set of ``actors``.

		For example, "*I need to monitor how many preys are killed by predators of this type. I will add a probe to track this information*".



Scenario
		A ``scenario`` is the representation of elements in the ``simulated world`` which are outside of the ``target system`` but may influence it.

		For example, "*My monsoon scenario, once combined to your epizootic scenario, shows unexpected results in terms of prey population.*"



Simulation
		A ``simulation`` corresponds to the actual execution of a ``simulation case``.

		For example: "*Yesterday I ran a predator / prey simulation on your computer*".



Simulation Case
		A ``simulation case`` is the description of a ``simulation``. This encompasses:

		  - technical settings, like the properties to be enforced for this simulation (ex: reproducibility) or the list of the eligible computing hosts

		  - domain-specific settings, like the description of the initial state of the simulation or the termination criteria

		For example, "*This predator / prey simulation case,  which must run on these following 3 computers, takes place in this kind of savannah and starts with 2 predators (that can mate) and 15 preys, on these specified locations. This is the weather scenario that I want to apply. I want the simulation to stop whenever either all animals are extinct or the elapsed duration (in simulation time) reaches one century.*"



Simulated World
		The ``simulated world`` corresponds to the whole context which encompasses the various elements that are evaluated through the simulation; the simulated world gathers both the target system and its context, i.e., respectively, model and scenario instances.

		For example, "*The simulated world is the whole savannah, including fauna and flora.*"



Simulation Inputs
		The ``simulation inputs`` correspond to the data which is needed for the ``simulation`` to be ready to start. This encompasses notably the description of its initial state, i.e. the data defining the state of the whole ``simulated world`` when the simulation is to begin.



Simulation Outputs
		The ``simulation outputs`` regroup the ``simulation results`` and the ``simulation traces``.



Simulation Results
		The ``simulation results`` are the main by-product of a ``simulation``, if not its only purpose. These are data that is computed based on information provided by ``actors``, at various points in ``simulation time``, and that are aggregated and managed by ``probes``.



Simulation Time
		There are at least two kinds of time that are useful in the context of a simulation: the wall-clock (user) time (i.e. the one we experience) and the ``simulation time`` that is known of the ``actors`` (a.k.a. the virtual time the ``simulated world`` is plunged into; at least a discretised version thereof).



Simulation Traces
		The ``simulation traces`` correspond to the timestamped information emitted by the ``actors`` and the technical agents of the ``simulation``, during its course. These are not ``simulation results``, they are a technical means of following the events that happen during a simulation, for example in order to troubleshoot the behaviour of ``models``.



Target System
	   The ``target system`` is the system of interest, whose mode of operation is reproduced thanks to a set of ``models``. Generally such a target system cannot be simulated without its context, i.e. parts of the reality that do no belong to the target system but must be taken into account to simulate it.

	   For example, "*The target system is the preys and the predators. Its context is the weather and the savannah (vegetation and relief).*"
