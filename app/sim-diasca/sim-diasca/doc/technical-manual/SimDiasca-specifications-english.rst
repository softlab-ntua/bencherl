:raw-latex:`\pagebreak`

-------------------------
Sim-Diasca Specifications
-------------------------


.. comment Requirements


Sim-Diasca has been originally designed in the context of two French and British projects aiming to perform a massive roll-out of communicating meters for millions of residential customers.

This became a typical example of the complex systems whose simulation could be addressed by Sim-Diasca, showing the usual process involved by a project having to focus on business simulations: as the development of models and simulation cases is long and expensive, and as switching from an engine to another in the course of a project is almost never an option, an appropriate choice in the simulation tools is crucial for the success of such a project.

To reduce this technical risk, a very basic, straightforward decision process is generally to be applied:

 #. establish the functional requirements for the targeted simulator
 #. deduce and formalise the corresponding technical requirements
 #. make a review of the state of the art of this simulation field, and establish a short list of the most relevant simulation tools
 #. test and rank each of them against the list of needed properties that was previously agreed from step 2 and weighted accordingly
 #. elect the best candidate, possibly by benchmarking it on a representative use-case against a reference tool or the best other contenders



Simulator Potential Role
========================

.. Note:: In this document, we will often rely on examples taken from the smart metering field; however one must keep in mind that Sim-Diasca is a fully generic, business-free simulation engine, which has no connection at all with metering, and which can be applied to *any* kind of discrete complex systems.


In our context, the goal was to be able to perform virtual experiments on a system - here, before it even exists - mainly for decision-making purposes.

Indeed, the main purpose for such a simulator is generally to allow for a harmless test of a system:


:raw-html:`<img src="xkcd-los_alamos.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-los_alamos.png}` [#]_


.. [#] For the non-native English speakers, expressions like "``SOH CAH TOA``" are `mnemonics <http://en.wikipedia.org/wiki/Trigonometry#Mnemonics>`_ for basic trigonometry.


(see the credits_ section about the comic strips)


Such a simulator could allow indeed to:

 - **determine a priori** some of the properties of the target system:

	- functionally, for example: "*How are implemented the business services, with what overall coverage and constraints on the processes?*"

	- technically, for example: "*What is the lowest/mean/highest possible duration for this particular request to be completed?*" or "*What is the minimal bandwidth to plan for this particular link?*"

 - **compare answers** to a call for tenders, so that the various solution candidates can be better evaluated

 - better **review delivered versions** of the system and **ease their technical validation**

 - **help developing and tuning** the system, for example by the impact on traffic due to a change in a data format

 - **help validating project strategies**, for example regarding the roll-out: "*If meters are installed without any particular order, what is the ratio of meters whose proper operation could be directly validated at installation-time?*"

 - **quantify the costs of the system** to establish its value analysis, by evaluating the total investments costs (ex: for an installed concentrator) and operational costs (ex: for GPRS communication costs, which may be partly proportional to the actual exchanged volumes)

 - better **secure the ability of the system to evolve**, for example by evaluating *a priori* the impact of a change in the functional or technical perimeter: "*Should this new service be offered, what would be its technical feasibility (ex: in terms of embedded processing or telecom link) and what would be the consequences on the overall properties and performance of the system?*"

 - **ease the test** of all or part of the system, for example by recreating the environment of an actual component thanks to the simulation, and by assessing the correctness of the behaviour of the component with regard to a set of test interactions, which comply or not to the specifications


Of course any given simulator will rarely be requested to fulfil everything in such a wide range of expectations, but these are examples of questions that could be tackled thanks to a generic enough simulator and a set of appropriate models and metrics.




Simulator Potential Context of Use
==================================

Beyond the operational context of the aforementioned projects, such a simulator could be applied to:

  - R&D studies of alternative architectures for a metering system, for example:

	- offering different services
	- and/or based on other choices in terms of software architecture (ex: showing a different dispatching of the processing on the various devices)
	- and/or using different infrastructures (ex: mesh networks)

  - other target systems, like the one developed by the EDF supplier, instead of the one developed by ERDF, the monopolistic distributor

  - the British case, as a tool to help decision-making (EDF Energy, CLEVER project)

  - more generally, all kinds of simulation of information systems

  - more generally, all kinds of simulation of complex discrete systems



Functional & Technical Requirements
===================================

Here the simulation objective is to be able to rely on a **simplified** version of the target system, i.e. a *model* of it, on which various experiments can be conducted so that the overall system can be better understood, and questions about it can be answered.

We will discuss here the main requirements that applied to our use-cases and thus played a main role in the design choices for Sim-Diasca.



A Key Point: Scalability
------------------------


Other Approaches Than Simulation Hardly Scale
.............................................

Among the most challenging questions raised by this new system, many of them were directly related to the consequences of its significant size. And this same size prevented most of the usual evaluation approaches to be applicable. Indeed these approaches, which include:

 - thought experiments
 - expert-based assessments
 - simple extrapolations
 - more complex spreadsheet-based computations

could hardly tackle non-trivial questions since they generally fail to recreate precisely what happens in the system (notably time-wise) and what are the outcomes of these corresponding interactions: usually, only macroscopic values at equilibrium or not depending on time can be expected from these approaches.

Indeed some questions become increasingly difficult and crucial to tackle as the size of the target system rises: even simple individual behaviours, once interacting with a sufficient number of others, can combine themselves to form complex systems whose behaviour is surprisingly difficult to predict.

Solving issues affecting these systems is all the more difficult than some elements of a metering infrastructure, like the concentrators or the PLC networks, are themselves complex.

Despite these difficulties, such scale effects must be addressed soon, as costs induced by their late detection become quickly prohibitive.

Therefore the use of more demanding approaches like *simulation* is often needed, since, more often than not, a real-size target system cannot be built just for test purpose.




A Simulator May or May Not Scale
................................

Due to the very large number of devices in most metering systems (more than 35 million meters in the French case), the simulator has itself to be able to scale up.

This does not necessarily imply that this tool must to be able to reach the exact full size of the target system, however it means it should be able at the very least to handle a massive numbers of interacting elements, as close as reasonably achievable to the real extent of the planned system.

Scalability is therefore at the heart of the properties wanted for that kind of simulators.


This concern severely constrained its implementation: so that it can reach performances suitable for its intended use, or just have a reasonable chance to actually deal with the problem in its required size, one had to ensure that the operation of the simulator is as **concurrent** as possible.



Concurrency First, But Other Properties Matter Too
..................................................

The simulator had thus to be designed to be strongly *parallel* (on a given computation node, multiple models can be evaluated simultaneously by the available cores) and *distributed* (a simulation can take place over a set of networked computation nodes), and all this without hurting the properties deemed important but difficult to preserve in that context, such as:

 - the correctness of the evaluation of models
 - the preservation of causality between simulation events
 - the ability to have completely reproducible simulations

More precisely, in our case the objective was to rely on a framework, made of a generic simulation engine and of reusable components, that allows the development of simulations of information systems that are:

 - **discrete** rather than continuous, because the modeled phenomena themselves are essentially discrete, and those which were continuous could easily be quantized

 - in **dynamic state** rather than in steady state, since for example cascading outages or the progressive roll-out of the system are subjects of interest

 - **event-driven**, as state changes of the modeled instances are generally punctual and can happen at any time

 - **causal**, so that a total order on the simulation events can be recreated despite the massive concurrency

 - **reproducible**, so that different executions of the simulation take place identically, no matter their execution context, i.e. not depending on scheduling, dispatching of processing, available resources, number and nature of computing nodes, capacity of the network, etc.

 - **intensely concurrent**, as already mentioned, thus supporting a high degree of parallelism (taking advantage of multicores and SMP [#]_) and able to be distributed over HPC [#]_ solutions like clusters or supercomputers (ex: ``Bluegene``)

 - **potentially of very large scale**, as already mentioned, to be able to simulate systems made of many thousands, if not millions, of interacting elements


.. [#] SMP: *Symmetric multiprocessing*.

.. [#] HPC: *High Performance Computing*.


This is the base specifications we had in mind for Sim-Diasca. However more generic and/or detailed requirements could be imagined, they are listed below.




List of Spotted Potential Properties For the Simulator
......................................................


Determining the simulation properties that are required is a critical step of a project, so that an appropriate engine can be chosen. Indeed, the requirements may include very varying features to be provided by such a simulation engine, from high-level programming of models to scalability or support for continuous components (i.e. solver embedding).

The devil is in the details in terms of tool selection as well. So even two discrete simulation engines that, from a remote point of view, might look rather similar, may actually be widely different beasts.


..
  disabled, as not fully SFW::
  raw-html:`<img src="xkcd-compare_and_contrast.png"></img>`
  raw-latex:`\includegraphics[scale=0.6]{xkcd-compare_and_contrast.png}`



Related to Simulation Correctness
_________________________________


 - **P1** Preservation of causality between events (see the `Maintaining Causality`_ section for detailed explanations)
 - **P2** Reproducibility of the evaluation of models (this is directly linked to the usability of the simulator: one usually needs to be able to relate changes in simulation results to changes operated on the target system or on its context)



Related to What Can Be Simulated
________________________________


 - **P3** Models are based on discrete events, even for any continuous phenomenon

 - **P4** Ability to simulate the system when it is in static/steady/nominal state

 - **P5** Ability to simulate the system when it is in any dynamic/transient/abnormal state, for example when being deployed, or under unexpected circumstances (ex: cascading failures), or during migration between versions

 - **P6** Ability to support stochastic actors, whose behaviours depend on a set of various random variables based on various probabilistic distributions (opens to Monte Carlo computations)



Related to Interaction With the Simulator
_________________________________________

 - **P7** Ability to run in batch (i.e. non-interactive) mode

 - **P8** Ability to run in interactive mode (for emulation and/or if human can be in the loop)

 - **P9** Use of a standardised format for simulation traces and results (to interface to third-party tools instead of having to develop them)



Related to the Size of the System That Can Be Simulated
_______________________________________________________

 - **P11** Ability to process, algorithm-wise (in terms of logic and expressiveness, not depending on the way we dispatch processing), in parallel most, if not all, models, instead of having them evaluated sequentially (ex: 5 million models running simultaneously rather than having 5 million models to walk through, one after the other)

 - **P12** Ability to take advantage of parallel computational resources, like SMP (multi-processors) and multicores (i.e. to dispatch a simulation over a set of local processing units)

 - **P13** Ability to take advantage of distributed computational resources (i.e. to dispatch a simulation over a set of networked computing nodes)

 - **P14** Ability to use HPC resources (full-blown clusters, super-computers, etc.)



Related to How Models Can Be Injected Into the Simulation
_________________________________________________________


 - **P15** Ability to add new models easily (extensibility)

 - **P16** Ability to define models with little effort, with a high-level modelling language (for example abstracting technical constraints, being based on an appropriate formalism, or even opening the use of advanced modelling tools, for model-checking, formal proof, etc.)

 - **P17** Ability to integrate with real devices (i.e. having actual equipments taking parts among models into simulations)

 - **P18** Ability to perform model composition, parameterised models, dynamic topology, multi-level evaluations, etc.


Related to the Technical Characteristics of the Simulator Itself
________________________________________________________________


 - **P19** Ability to interface easily to third-party tools (ex: to an emulation layer of a specific protocol, to post-processing tools, etc.)

 - **P20** Use of free software tools (thus that can be modified/fixed/enhanced/shared/freely used), preferably well-known



Newly Added Properties
______________________

These properties and features were not listed in the initial requirements, but over time proved to be key points as well:

 - **P21** Support for a complete result management, which allows mainly the user to specify what are the results expected from the simulation (preferably producing them, and only them) and then automatically collects and retrieves them to the user node, efficiently (ex: post-processing them concurrently on the computing nodes, and sending corresponding compressed data over the network) and conveniently (ex: gathering everything in a experiment-specific directory on the user node, and allowing to browse them automatically if not in batch mode)

 - **P22** A basic support for simulation reliability is to be provided: first of all, results will be produced if and only if the simulation not only terminates, but terminates on a success; otherwise, as soon as any of its elements fail (including model instances), the simulation should crash immediately and completely (as a whole); any abnormal slow-down should be reported, and a diagnosis system should be provided, notably to help the debugging of models (who are the lingering instances, what are they doing, who are they waiting for, etc.)
