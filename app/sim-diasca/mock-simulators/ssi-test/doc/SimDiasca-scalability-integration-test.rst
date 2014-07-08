.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


==============================================
SSI-Test in the Context of the Mock Simulators
==============================================

----------------------------------------------------------
Description of the Sim-Diasca Scalability Integration Test
----------------------------------------------------------





:Author: Jingxuan Ma
:Contact: jingxuan.ma@edf.fr
:Organization: Copyright (C) 2011 EDF R&D
:Creation Date: September 2010
:Status: Work in progress
:Version: 0.2
:Dedication: For people who are interested in the *Sim-Diasca* simulation engine, and in particular for those who want to have an overview of its features and test them.
:Abstract: SSI-Test is designed as a business-free Sim-Diasca integration test case, allowing to illustrate and test the main features of the engine.
The requirements, design and implementation as well as the user guide of this test are specified in this document.



.. meta::
   :keywords: Sim-Diasca, massive, simulation, business-free, integration, test



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 2

.. section-numbering::





:raw-latex:`\pagebreak`





Overview and Context
====================

SSI-Test stands for *Sim-Diasca Scalability Integration Test*.

SSI-Test is a business-free Sim-Diasca integration test case. Its implementation is based on the Sim-Diasca distributed versions.

The SSI-Test objectives are:

 - to illustrate how a simulation based on Sim-Diasca works with a complete and general test example: the life-cycle of the simulation and of its actors, and the interactions between models and the simulation framework

 - to allow to check the main simulation properties (causality, reproducibility, etc.)

 - to provide a way of showing and assessing, testing and challenging the scalability of Sim-Diasca with some scale-related settings, such as simulation duration, total actor count, etc.




SSI-Test Requirements
=====================

**In term of giving an general illustration of Sim-Diasca**, SSI-Test must be a complete, general and with no any business constraint test, which can be used for the illustration and test of the Sim-Diasca distributed versions.


**In term of technique**, SSI-Test must illustrate and allow to check following features.



Configuration of a Specific Simulation
--------------------------------------

SSI-Test must allow to illustrate the various ways of configuring a simulation, with regard to the:

 - simulation settings: simulation name, fundamental simulation frequencies, etc.
 - deployment settings: what are the computing resources that should be used
 - load-balancing settings: which heuristic algorithm is used for actor placement, although only round-robin is available


Creation of Simulation Actors
-----------------------------


 - create actors initially before simulation is started: this kind of actor must be **created before** the simulation starts and **after** deployment manager creation; and is done **by calling** "class_Actor:create_initial_actor" with "class_Name", "Construct Parameter-list" and the "load-balancer pid" in option

 - create actors at runtime, i.e. in the course of simulation: this kind of actor is **created after** the simulation starts **by calling** ``class_Actor:create_actor`` with ``class_Name``, "construct parameter-list" and the "instantaneous state"; it can be only done by another sim-diasca actors and so it is generally integrated in actors spontaneous activities.



Scheduling of Actor's Activities
--------------------------------

The actors with three scheduling modes respectively are designed:

At least three usual scheduling profiles should be illustrated:

 - one in periodic mode: its attribute future_action is set always as current tick + a predefined periodic. It runs its spontaneous behaviors periodically

 - one in passive mode: its attribute future_action is always passive, so it has no any spontaneous behavior, it is triggered by messages and reacts according to the received messages

 - one in arbitrary-scheduled mode: its attribute future_action is set as current tick + periodic while this periodic is not static. The actor can have its regular spontaneous behaviors, but on the other hand, its behaviors and the periodic can be changed with the received message


Inter-Actor Communication
-------------------------

The actors can communicate only by calling ``class_Actor:send_actor_message/3``.

In case of a communication with a attached message, the calling   is as:
class_Actor:send_actor_message(DestinationPid, {called method, message}, instantaneous state).

In case of no any attached message, the calling is like:
class_Actor:send_actor_message(DestinationPid, called method, instantaneous state).



Actor Termination
-----------------


It should be mentioned that the termination of an actor signifies here a normal termination, but not a crash of any simulation failure.

An actor can be terminated:

  - with simulation termination
  - because of its longevity
  - because of its specific behavior triggered by some received messages

Anyway, when an actor is terminated normally, only its process is deleted from the simulation and the simulation continues.

And the termination of an actor is done by simply executing: executeOneway(AppropriateState, triggerTermination) in its spontaneous activities.

Simulation Termination
----------------------


The simulation can be terminated when:

  - the termination offset is reached

  - any predefined termination constraint is satisfied

 Anyway, when a simulation is terminated, all its associated processes are deleted.

 And the termination of a simulation is managed by the time manager.


Scalability Measurement
-----------------------

As each actor can have its longevity, an actor can be designed to create other
actor and also be terminated in simulation time, so the scalability can be
controlled by setting initial actor number, simulation predefined duration and
also by some spontaneous behaviors concerning new actor creation and other actor
termination.



:raw-latex:`\pagebreak`

A SSI-Test Case Implementation
==============================

To fulfill the requirements listed above, a SSI-Test case example has been defined.

In order to ensure a complete example with no any business constraint, this test case simulates a forest ecosystem scenario in which the forest dwellers in particular, squirrels and oaks, generate, grow, communicate each other, react according to others' spontaneous behaviors and the life of the forest and its dwellers depends on each other, for example, the forest will be destroyed when no any dweller or destroyed of a fire, all forest dwellers terminate when a forest is destroyed.


Technique implementation
------------------------

 This SSI-Test case is realized on following tools:

 - Sim-Diasca version 2.0.9

 - wooper 1.0

 - traces 0.3

 - Erlang R14A

 - java 1.6.0_13


Models implementation
---------------------

 Based on above technique tools, a principal test module and four modules with three type actors are implemented as following:

 - ssi_test.erl: principal test module in which the test start, the simulation configurations, the initial actor creations as well as the test stop are implemented.

 - Forest Dweller: an abstract model implemented in "class_ForestDweller.erl", derived directly from class_Actor.erl and including some common attributes and behaviors of forest dweller. This model can not be directly instantiated.

 - Oak: implemented in ``class_Oak.erl``, derived from class_ForestDweller.erl. It is defined as a passive scheduling mode actor and all specific oak attributes and spontaneous behaviors are defined.

 - Squirrel: implemented in "class_Squirrel.erl", derived from class_ForestDweller.erl. It is defined as a complexe scheduling mode actor and all specific squirrel attributes and spontaneous behaviors are defined.

 - Forest.erl: implemented in "class_Forest.Erl", derived from class_actor.erl and is composed by Forest Dwellers. Is is defined as a periodic scheduling actor.
