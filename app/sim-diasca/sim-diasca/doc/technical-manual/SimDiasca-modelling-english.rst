:raw-latex:`\pagebreak`


------------------
Modelling Approach
------------------


:raw-html:`<img src="xkcd-physicists.png"></img>`
:raw-latex:`\includegraphics[scale=0.65]{xkcd-physicists.png}`



Questions, Metrics & Models
===========================

Depending on the question one is asking about the target system (ex: "*What is the mean duration of that service request?*") or on the properties one wants to evaluate (ex: robustness, availability, cost, etc.), **metrics** (a.k.a. *system observables*), which are often macroscopic, system-wide, have to be defined. For example: round-trip time elapsed for a service request, or yearly total operational cost for the GPRS infrastructure.

These specific metrics will have to be computed and monitored appropriately during the corresponding simulations.

To do so, the behaviour of the elements of the system that have an impact on these values must be reproduced. For example,  when evaluating a distributed information system, business-specific elements like devices, network components, etc. must be taken into account.

The simulator will manage all these elements based on simplified representations of them (*models*), selected to be relevant for the behavioural traits which matter for the question being studied.


Therefore the choice of the elements to take into account will depend on the selected metrics: if for example one is only interested in the volume of data exchanged in nominal state, deployment policies or failures of some devices might not be relevant there. On the contrary, evaluating reliability may not require to go into deeper details about each and every exchanged byte on the wire.

Moreover, the same element of the target system might be modeled differently depending on the chosen metrics: for example, for a network-based metrics, a device may be modeled merely as a gateway, whereas for reliability studies it will be seen as an equipment able to fail and be repaired according to stochastic models.


.. Note:: Simply put, a question asked to the simulator results in a choice of metrics, which in turn results in a choice of appropriate models.


Therefore each element of the system can be represented by a set of models, of various nature and level of detail, results of the work of experts on various subjects, according to the following theoretical diagram:

:raw-html:`<img src="modelling-approach-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{modelling-approach-english.png}`

Different fields of expertise (notably functional and technical experts) have to define the simulation goals, metrics, and to work collaboratively on a set of common models, which form the modelling framework.

Once specified, the pool of models can be reused at will in the context of different experiments: these models can be assembled together and projected in various execution environments, with different purposes and level of detail.




Implementation Of Models
========================

Uncoupling as much as possible models from implementations allows to reduce the dependency on a specific set of simulation tools, even though inevitably constraints remain.

In the case of the AMM project, two completely different simulation environments were developed, based on a common view of the system:

 - **AMM-Jade**, making use of the `Jade <http://jade.tilab.com/>`_ multi-agent system, for fast prototyping purposes

 - **Sim-Diasca**, discussed here, making use of `Erlang <http://www.erlang.org/>`_ and of various custom-made layers, for finer modelling and HPC simulation purposes


These two threads of activity did not share any code but had a common approach to modelling.

In both cases, simulations operate on *instances* of models.

A model must be understood here in its wider sense: real system elements, such as meters or concentrators, are of course models, but abstract elements, like deployment policies or failure laws, can be models as well. Basically every stateful interacting simulation element should be a model, notably so that it can be correctly synchronised by the simulation engine.

A model can be seen roughly as a class in object-oriented programming (OOP).

Then each particular element of the simulation - say, a given meter - is an instance of a model - say, the ``AMM Meter`` model.

In agent-based simulations like the ones described here, all instances communicate *only* by message passing, i.e. shared (global) variables are not allowed. Added to code that is free from side-effects, seamless distribution of the processing becomes possible.

Unlike OOP though, the instances are not only made of a state and of code operating on them (methods): each and every instance also has its own thread of execution. All instances live their life concurrently, independently from the others. This is the concept of `agents <http://en.wikipedia.org/wiki/Intelligent_agent/>`_.

Relying on them is particularly appropriate here, as the reality we want to simulate is itself concurrent: in real life, the behaviour of all meters is concurrent indeed. Thus using concurrent software is a way of bridging the semantic gap between the real elements and their simulated counterparts, so that models can be expressed more easily and executed more efficiently.

Besides, these agents have to follow some conventions, some technical rules, to ensure that the aforementioned list of simulation properties can be met.

We finally call such a well-behaving agent a *simulation actor*, or simply an *actor*.
The simulator can therefore be seen as a set of technical components that allow to operate on actors, notably in order to manage their scheduling and communication.

This topic is directly in relation with the issue of time management, which is discussed below.
