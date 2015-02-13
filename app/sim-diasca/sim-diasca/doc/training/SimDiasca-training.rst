=============================
`Sim-Diasca` Training Session
=============================


+-------------------------------------+---------------------------------+
| .. image:: sim-diasca.png                                             |
|   :scale: 30                                                          |
|   :align: center                                                      |
+=====================================+=================================+
| .. image::    logo-EDF-english.png  | .. image::    lgplv3-147x51.png |
|   :scale: 50                        |   :scale: 20                    |
|   :align: left                      |   :align: center                |
+-------------------------------------+---------------------------------+


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Author: Olivier Boudeville
:Contact: olivier.boudeville@edf.fr
:Organisation: Copyright (C) 2008-2013 EDF R&D
:Last Update: Monday, March 4, 2013
:Status: Work in progress
:Version: 0.7
:Dedication:

	For people having to develop models and simulation cases with Sim-Diasca.


:Abstract:

	The main information needed to develop with Sim-Diasca is given in this document.

	The scope is broad, as it should cover everything from the mere understanding needed by end-users to a more in-depth knowledge about all the technical layers involved.

	Such a deeper understanding is very useful whenever debugging or tuning simulation-specific models.




.. meta::
   :keywords: Sim-Diasca, massive, simulation, multi-agent, development, training, exercises


:raw-latex:`\pagebreak`

.. contents:: Sim-Diasca Training
	:depth: 2

.. section-numbering::







:raw-latex:`\pagebreak`

Program
=======

The first training was organised as an informal two-day workshop, with alternating presentations and short exercises/practical works.

.. comment:
 - Monday, May 18, 2009, from 9:30 to 18:30 (followed by a restaurant)
 - Tuesday, May 19, 2009, from 9:30 to 18:30

  Light breakfast each day at 9:15 - 9:30.

  Lunch break around 12:00 at the local canteen, coffee at about 13:00.

  The training will take place in our Clamart R&D site (reception: +33 (0)1-47-65-43-21, `access map <http://rd.edf.com/fichiers/fckeditor/File/Plan-Clamart.pdf>`_), whose address is::

  1, avenue du Général de Gaulle
  92140 Clamart
  France

.. comment:
  Precise location in the site is building B, in room B-001 (ground floor). The phone number of this room is ``+33 (0)1-47-65-27-01``.

Printed copies of the latest training material should be available at training start-up.

In case of trouble, here are our full coordinates:

 - Olivier Boudeville, SINETICS department, ASICS group, office in B-226

  - office phone: +33-1-47-65-59-58, mobile: +33-6-16-83-37-22 (France prefix is +33),
  - professional mail: ``olivier.boudeville@edf.fr``
  - personal mail: ``olivier.boudeville@online.fr``

 - Jingxuan Ma, SINETICS department, ASICS group,

  - office phone: +33-1-47-65-54-15
  - professional mail: ``jingxuan.ma@edf.fr``






:raw-latex:`\pagebreak`

Training Overview
=================

The goal of this two-day training is to be able to:

 - write simulations:

	- implement models
	- implement test cases and simulation scenarios

 - run simulations
 - debug simulations
 - understand the inner workings and be able to troubleshoot most usual problems
 - have a better understanding about what are the services offered by the simulation engine

The training will consist on **technical presentations** with their corresponding set of short **10 exercises/practical works**, of increasing difficulty, to be done by trainees.

The correction of each exercise will be given once that exercise will be completed by the trainees.


.. Note:: A lot of ground is to cover, and such a leap is difficult to perform in only two days. Therefore it might be felt as a bit overwhelming, near the end of the training. Nevertheless, after some practise, writing models is quite straightforward and involves only a very limited number of conventions to follow.




:raw-latex:`\pagebreak`

Prerequisites
=============

For the practical work, each attendee should bring with him a 32-bit or 64-bit laptop running a recent GNU/Linux operating system. Upgrading it, prior to the training, to the latest stable version is recommended. Ideally this would be a multi-core laptop running a Debian-based distribution (ex: Ubuntu).

One is encouraged to bring along also one's copy of the *Programming Erlang* book, authored by Joe Armstrong.

A prior reading of the `WOOPER documentation <http://ceylan.sourceforge.net/main/documentation/wooper/>`_ and of the ``Sim-Diasca Technical Overview`` (see file below) is strongly recommended for this Sim-Diasca training, otherwise the two-day schedule would be most probably too tight for the fields to cover.


We will rely mostly on the following files, supplied at the beginning of the training:

 - Erlang files:

   - ``otp_src_R16B.tar.gz``: contains the sources of the current Erlang environment
   - ``otp_doc_html_R16B.tar.gz``: contains the documentation of the current Erlang environment

 - WOOPER file: ``wooper.pdf`` contains the latest full official documentation for that layer

 - Sim-Diasca files:

   - ``SimDiasca-training-install.tar.bz2``: contains all the sources for WOOPER and Sim-Diasca (includes the Erlang installation script)
   - ``SimDiasca-technical-overview-english.pdf``: a technical overview of the `Sim-Diasca` simulation engine, to be kept as a reference
   - ``SimDiasca-training-elements.tar.bz2``, which contains ``SimDiasca-training.pdf`` (this document) and various files needed for some training exercises

The ``SimDiasca-training-corrected-exercises.tar.bz2`` archive, containing the text of the correction (``SimDiasca-training-corrected-exercises.pdf``) and the corresponding source files will be distributed at the end of the training.


Following prerequisites should already be installed on the trainee laptop:
gcc make libncurses5-dev openssl libssl-dev

 - gcc
 - gnuplot
 - eog
 - libncurses development package (ex: ``libncurses5-dev``, for Debian-based distributions)
 - SSL runtime and development packages (ex: ``openssl``, ``libssl-dev``)
 - dot
 - ant
 - java JDK environment (i.e. including a compiler, not only an interpreter)
 - svn (client)
 - make (GNU version)
 - gkrellm
 - any program editor (ex: nedit, emacs, `erlIDE <http://erlide.sourceforge.net/>`_, etc.)
 - any PDF reader (ex: evince)
 - optional at this stage: just testing, before the training, a `LogMX <http://www.logmx.com/>`_ install would allow to check it can properly work on the trainee platform indeed [#]_ (see also the *Installing LogMX* section in the Sim-Diasca technical overview)


.. [#] Some infrequent problems were encountered on some specific Linux installations, that would have to be updated and/or configured appropriately.


.. Note:: Unfortunately we are not allowed to let third-party computers (not managed by EDF) connect to our internal network. Please download and install everything needed beforehand. During the training, just tell us what you would need, so that we can download it for you and t.






:raw-latex:`\pagebreak`

Simulation Objectives
=====================

We will speak here of the **requirements**, not of the ways of fulfilling them.

With Sim-Diasca, **developers** should be able to:

 - define models, in terms of states and behaviours, relying on usual UML-based modelling approaches (notably: class diagrams, sequence diagrams)

 - put together models and make them interact according to a simulation scenario


**End-users** should be able to:

 - select a simulation case and run it
 - study its results and possibly change some of the scenario or model parameters
 - re-run the simulation case with these newer settings
 - be sure that any change observed in the outputs results actually from a change in the input parameters

Then the impact of any given parameter change onto the studied trajectory of the system can be safely determined by the system designer, regardless of any technical context.

This implies preserving *reproducibility*.


.. admonition:: Example

  Reproducibility is the key when wanting, for instance, to fine-tune the quantity of fuel for a rocket launch in a given set of detailed conditions (ex: wind, altitude, temperature, etc.).



End-users should also be able to:

 - select a simulation case and to run it any number of times [#]_, in order to explore as many licit possible outcomes for that scenario, and to sample these outcomes fairly, i.e. so that two equally likely situations according to the models have the same probability of occuring in simulations

 - run any relevant post-processing to establish statistical learning from these run experiments

.. [#] Usually it will be a statistically-meaningful (large) number of experiments; see also the `Monte Carlo <http://en.wikipedia.org/wiki/Monte_Carlo_method>`_ methods.

Then the simulation user can figure out not only the possible futures of the simulated system, but also their respective probability.

This implies preserving *ergodicity* [#]_.


.. admonition:: Example

  Ergodicity is the key when wanting to proceed, for instance, to thousands of virtual launches of a rocket, with varying conditions (quantity of fuel, weather, launch location, events, etc.) and determining the overall probability that the launch succeeds.


.. [#] Defined here as the possibility for the simulation system:

 #. to reproduce all the licit combinations of events that may happen according to the models (full exploration)
 #. and to ensure that all combinations that are equally likely in the modelling will have the same probability of occuring in the simulations (fair exploration)


As a consequence, although each combination of events in ergodic mode is generally perfectly reproducible, in general a simulation is executed with the intent:

 - either to operate on one specific trajectory, with its parameters tweaked one at a time from a run to another: then reproducibility is wanted

 - or on the contrary to examine as many trajectories of the system as possible, explored run after run: then ergodicity is wanted

This is why, from the point of view of the simulation user, reproducibility and ergodicity are antagonistic (although any given situation in ergodic mode can be replayed as wanted, in a reproducible manner).

Finally, simulations may have to involve a very large number of actors (several hundreds of thousands), and, of course, every simulation has to be correct, i.e. to be an exact result deduced from the models, and from them only (ex: not depending on any technical context).

We will see later in this document why the technical answer proposed by Sim-Diasca covers these requirements.






:raw-latex:`\pagebreak`

Overall Technical Architecture
==============================

A simulator based on Sim-Diasca relies on four layers, from bottom to top:

 #. Erlang
 #. WOOPER
 #. Sim-Diasca
 #. Simulation-specific models

A set of third-party tools is also used.

The resulting architecture can be represented that way:

:raw-html:`<img src=technical-architecture-english.png></img>`
:raw-latex:`\includegraphics[scale=0.5]{technical-architecture-english.png}`






:raw-latex:`\pagebreak`

Erlang
======

This is the very first layer of the simulation architecture.


Learning The Basics
-------------------

This task has been done during the `Erlang Training and Consulting <http://www.erlang-consulting.com>`_ workshop, taught by Oscar Hellstrom.


.. comment:
 - what was done after Jingxuan and I left?
 - general feedback of the attendees about the Erlang training
 - any later questions regarding basic Erlang use?

:raw-latex:`\pagebreak`



Beyond The Basics
-----------------

Quite a lot of more advanced topics could not be discussed during the Erlang training, short of having enough time.

They are listed below from the least interesting in the current context to the most.


Remaining technical subjects that are not directly linked with the simulation needs
.......................................................................

 - **bit syntax**: for bit-level operations
 - **monitors**: asymmetric links
 - **recursive fun**: using a Y-combinator to define them (see exercise ``ex_monitor``)
 - **process dictionary**: each process embeds a private associative table (use of it is almost never recommended, as it adds process-level side-effects)
 - **references**: quasi-unique worldwide distributed identifiers
 - **OTP** framework (a whole subject by itself): ``gen_event``, ``gen_server``, ``gen_fsm``, error logging, supervision, etc.
 - **socket programming**: IP, TCP, UDP, ``gen_tcp``, ``gen_udp``
 - **advanced features** for: real-time operations, very high industrial reliability, dynamic code loading without any interruption of service, etc.




:raw-latex:`\pagebreak`

Remaining technical subjects that might be useful in general and/or in a simulation context
..........................................................................

 - **list comprehensions**: to construct lists from generators and filters (see exercise ``ex_monitor``); they are tremendously convenient

 - **``try...catch``**: to manage properly functions that may raise exceptions (the training discussed the old-fashioned way of managing exceptions)

 - **crash dumps**: for post-mortem analysis of a crash of the virtual machine (Erlang-level stop on application failure, completely unrelated to the crash of the corresponding UNIX process, which almost never happens)

 - **process scheduling priorities**: they can be set to some extent, although generally it is not necessary

 - **interfacing techniques**: port, linked-in drivers, etc. to integrate with other languages (C, C++, Java, etc.) and have access to third-party libraries (ex: GUI, post-processing tools, etc.)

 - **file-system management**: files, directories, for reading and writing data

 - **ETS**, **DETS**: to store efficiently large amounts of structured data

 - **data formats** for protocol exchanges and persistence: ASN.1, XML, custom-made protocols, JSON, etc.

 - **Mnesia**: real-time *distributed* database, in memory and/or on disk, with fragmentation, replication, fault-tolerance, possible transactions, and management of *any* Erlang term (ex: object-oriented database)





:raw-latex:`\pagebreak`

Remaining technical subjects that are definitively appropriate when dealing with planned simulations
...............................................................................

 - **hash-tables**: efficient associative table (see exercise ``ex_interpreter``)

 - **macros** (see WOOPER section)

 - **binaries**: to optimise the management (storage in memory, exchange and sharing between processes) of potentially large chunks of data, including strings

 - **interesting modules**: ``string, list, os, digraph, dict, dets, ets, file, io, random``; a useful measure: having a direct bookmark pointing to the *Manual Page Index* corresponding to the current version of Erlang in use (see next section)
 - **local/global registration**




:raw-latex:`\pagebreak`

Recommended Use of Erlang in a Sim-Diasca Context
-------------------------------------------------


Streamlined Erlang Installation
...............................

This is automatically enforced by the use of the ``common/conf/install-erlang.sh`` script.

Another useful step is to enable proper syntax highlighting, both for Erlang and for WOOPER.


.. admonition:: Exercise ex_erlang_install

 - go into the directory where Sim-Diasca related developments are to be put, called from now though the supposedly set PREFIX environment variable (relative paths are then based on PREFIX); ex: ``export PREFIX=~/Sim-Diasca-testing``

 - extract the Sim-Diasca archive: ``tar xvjf SimDiasca-training-install.tar.bz2``

 - have a short review of the Sim-Diasca Erlang installation script, which is: ``common/conf/install-erlang.sh``, and run it with the ``--help`` option

 - run it to automatically install the Erlang environment: ``common/conf/install-erlang.sh --cutting-edge --doc-install --no-download``, from a directory where the ``otp_*`` files are available (no connection available from the training room)

 - update your PATH variable (preferably permanently, ex: in ``~/.bashrc``, one can add: ``export PATH=~/Software/Erlang/Erlang-current-install/bin:$PATH``)

 - run the interpreter from an updated environment (ex: ``source ~/.bashrc``) and check everything is correct (version, SMP, etc.): run ``erl``

 - recommended: bookmark the relevant documentation, a *Manual Page Index* bookmark should point to: ``~/Software/Erlang/Erlang-current-documentation/doc/man_index.html`` (the bookmark will remain valid even when the Erlang version will be updated)

 - optional:

  - update the nedit configuration file with the supplied one: ``mkdir -p ~/.nedit ; cd ~/.nedit; rm nedit.rc; ln -s $PREFIX/common/conf/nedit.rc``
  - display a WOOPER source file to check the syntax highlighting: ``nedit wooper/examples/class_Cat.erl``
  - then, congratulations, you should have a fully working Erlang environment!





:raw-latex:`\pagebreak`

Interlude: A Short Discussion About The Build System
....................................................

(necessary to understand the Erlang compiler settings)

 - the same build system is used for the ``common`` sub-layer, the WOOPER layer, the Sim-Diasca layer; possibly it could/should be used by any simulator based on Sim-Diasca

 - the build not based on Emakefiles (which are too limited)

 - it is based on GNU make instead, see `its official manual <http://www.gnu.org/software/make/manual/make.html>`_

 - it automates most if not all repetitive operations, and allows to perform them in parallel whenever possible (e.g.: often)

 - it does that with generic rules (no per-file rules to write)

 - all the main make targets are implemented: ``clean, doc, all, test``, etc.

 - the build files are: ``GNUmake*``, i.e. ``GNUmakesettings.inc``, ``GNUmakevars.inc``, ``GNUmakerules.inc``, all ``GNUmakefile`` files

 - its goals are:

	- minimising the information entered for each module
	- ensuring everything is managed the same, with best settings determined once for all, yet being customisable at will

 - the most problematic shortcoming: lack of management for dependencies between modules

   - no standard approach yet (corresponding ``erlc`` flag still to come)
   - most complete solutions are still a bit complicated
   - current practise: just issuing ``make`` (or a shorthand, like with ``alias m=make``) from the top-least (i.e. less deep) directory that encompasses all changes performed since the last compilation, or, simply, from the root of the developments


.. admonition:: Exercise ex_make

 - go into the ``common/src`` directory
 - issue: ``make clean all test``
 - interpret outputs
 - inspect all files in directory
 - see the ``common/GNUmakerules.inc`` to have a look at the ``clean all test`` targets
 - execute, in parallel with the reading of the corresponding rules:

   - ``make clean``
   - ``make all``
   - ``make test``

 - optional: add an alias to your shell configuration file: ``alias m=make``
 - see also: ``common/GNUmakevars.inc`` and at least one ``GNUmakefile`` (they will be commented and explained to you); for example: any test for a module named ``X`` (thus in ``X.erl``), is to be stored in ``X_test.erl``, and can be automatically compiled and run with proper settings thanks to: ``make X_run``


:raw-latex:`\pagebreak`




Streamlined Configuration of the Erlang Compiler
................................................


The Erlang compiler (``erlc``) allows to transform Erlang source files (``*.erl`` files) into precompiled byte-code files for the Erlang virtual machine (``*.beam`` files).

Its proper configuration is automatically enforced by the use of the WOOPER-based build system (which includes the ``common`` first sub-layer):

 - compiler options are defined incrementally in the per-layer ``GNUmakevars.inc`` file
 - options are referenced in rules defined incrementally in the per-layer ``GNUmakerules.inc`` file
 - the per-layer ``GNUmakesettings.inc`` file allows to order appropriately the definition of variables and rules of all lower layers
 - corresponding commands are finally triggered thanks to each per-directory ``GNUmakefile`` file, when running ``make``


.. admonition:: Exercise ex_compiler

 - go into the ``common`` directory

 - edit the ``GNUmakerules.inc`` file

 - search the ``'%.beam: %.erl'`` generic rule (this is pattern-matching again, this time in the "make" language!)

 - in its body, remove the ``@`` before ``$(ERLANG_COMPILER)`` so that the run command-line will be displayed in the terminal whenever executed

 - go into the ``common/src`` directory

 - run: ``make clean hashtable.beam``

 - see what is the actual command-line

 - all compiler flags will be described and explained (they are listed in the exercise correction)

 - restore the ``@``






:raw-latex:`\pagebreak`

Streamlined Configuration of the Erlang Interpreter
...................................................

The Erlang interpreter (``erl``) allows to run on the current platform any precompiled byte-code file for the Erlang virtual machine (``*.beam`` file, produced by the Erlang compiler just mentioned).

The proper configuration of the Erlang interpreter is automatically enforced as well, thanks to the use of the WOOPER-based build system (which includes the ``common`` first sub-layer).

To configure appropriately the Erlang virtual machine, a script is used by the all the generic rules having to run the interpreter: ``common/src/launch-erl.sh``.

The following exercise is also useful to understand later how:

 - WOOPER and Sim-Diasca actually manage notably per-instance state variables
 - test cases can be written


.. admonition:: Exercise ex_interpreter

 - go into the ``common`` directory
 - ``make clean all`` to rebuild everything from scratch
 - go into the ``common/src`` directory
 - have a look at the services offered by ``hashtable.erl``
 - see how ``hashtable_test.erl`` allows to perform some tests on the hashtable module
 - run: ``make hashtable_run``
 - check that the test succeeds
 - make the test fail by changing in ``hashtable_test.erl`` the expected value returned by a pattern-matching and then by running again ``make hashtable_run``
 - check the test now fails
 - restore the previous expected value
 - examine:

  - the ``common/src/launch-erl.sh`` script
  - the VM settings, as run on the command line

 - all interpreter flags will be described and explained (they are listed in the correction of the exercise)






:raw-latex:`\pagebreak`

Some Basic Good Practises
.........................


Tests
_____

For all modules defined, a corresponding module performing unit testing should exist: if adding ``X.erl``, then ``X_test.erl`` should also be defined.


Warnings
________

When a module is compiled (with the proper options), **no warning at all** should be issued. Otherwise useless debugging time will be spent on fake problems, as *all* warnings denote mistakes and misuses. This is why we ensured a warning blocks the build.


Runtime Monitoring
__________________

Some monitoring of the computer resources should be done regularly when running test cases, to check the behaviour of simulation elements.

.. admonition:: Exercise ex_monitor

  - run a system load monitor: ``gkrellm &``
  - get used with the meaning of the various displays
  - launch the interpreter and run this anonymous recursive function::

	  spawn(fun() -> F = fun(F) -> F(F) end, F(F) end).


  - interpret result thanks to the monitoring
  - launch the interpreter and run this list comprehension::

	  [ spawn(fun() -> F = fun(F) -> F(F) end, F(F) end) || X <- lists:seq(1,erlang:system_info(schedulers)) ].

  - interpret result thanks to the monitoring, regarding the use of SMP by Erlang



:raw-latex:`\pagebreak`

Basic Parallelization Pitfalls
______________________________

Some words about common problems encountered when designing parallel operations:


 - `concurrency control <http://en.wikipedia.org/wiki/Concurrency_control>`_:

  - definition: *solutions so that concurrency cannot violate data integrity*
  - must be taken care of whenever there can be multiple writing accesses to a given resource
  - most languages (C, C++, Java, Python, etc.) have to rely on specific synchronisation constructs (ex: mutexes, semaphores, serialisation, etc.)
  - their use is intensively error-prone, debugging parallel and/or distributed systems can be soon a nightmare (non-reproducible bugs, `Heisenbugs <http://en.wikipedia.org/wiki/Unusual_software_bug#Heisenbug>`_, etc.) and only very skilled developers are able to produce correct code
  - these problems usually cannot happen in Erlang since no data is shared between processes, and processes exchange only asynchronous messages


 - `deadlocks <http://en.wikipedia.org/wiki/Deadlock>`_:

  - definition: *a specific condition when two or more processes are each waiting for each other to release a resource, or more than two processes are waiting for resources in a circular chain*
  - example: one pencil, one rule, two persons wanting simultaneously to draw a line

  - with Erlang, only the unavoidable deadlocks can happen, i.e. the ones introduced by the user at the logic, at the protocol level (the Erlang VM is free from lower-level deadlocks); ex: when implementing the `Dining philosophers problem <http://en.wikipedia.org/wiki/Dining_philosophers_problem>`_


 - `race conditions <http://en.wikipedia.org/wiki/Race_condition>`_:

  - definition: *flaw in a system or process whereby the output and/or result of the process is unexpectedly and critically dependent on the sequence or timing of other (unrelated) events*

  - examples:

   - one process first spawning another one which is to run a faulty function, and then trying to create a link to it (the first process may or may not crash): this is why an atomic ``spawn_link`` function exists

   - two processes started roughly at the same time, one registering, the other wanting to interact with the first and therefore performing a look-up of its name: a total order on operations should be enforced, so that their outcome does not depend on the technical context, i.e. on the process scheduling, or on whether the computer happens to be under a light load, etc.

  - with Erlang, only the unavoidable race conditions can happen, i.e. the ones introduced by the user at the logic, at the protocol level (the Erlang VM is free from lower-level race conditions); see the two previous examples






:raw-latex:`\pagebreak`

Later Erlang Skills To Develop
------------------------------

 - profiling

 - tuning: see also the `Erlang Efficiency Guide <http://erlang.org/doc/efficiency_guide/part_frame.html>`_

 - increasing reliability (ex: OTP might be interesting to use)

 - data management and transformation, depending on what are the input data and what results are output, and on their volume (ex: large arrays, spreadsheets, databases, etc.)

 - graphical user interfaces, to enter simulation parameters or view the results (pre and post processing); can be done either natively or by interfacing with other tools/languages

 - probably a lot of topics will appear here during the course of simulation projects!


:raw-latex:`\pagebreak`








:raw-latex:`\pagebreak`

WOOPER
======

The WOOPER layer is the second of the simulation architecture, just on top of the Erlang environment.

Is there any attendee who has not read the WOOPER documentation yet?


Mode Of Operation
-----------------

There is a one stop only, for all the user documentation: the `WOOPER official documentation <http://ceylan.sourceforge.net/main/documentation/wooper/>`_, a.k.a. the "WOOPER Bible". Hopefully, as recommended in the prerequisite section, the attendees will have already read it.



General Principle
.................

In a nutshell, WOOPER implements each high-level object (instance of a class) as an Erlang process that respects the usual form of an Erlang server::

  start(Args) ->
	InitialState = f(Args),
	spawn( ?MODULE, loop, InitialState ).

  loop(State) ->
	receive

		{command,x_request} ->
			NewState = apply_X(State),
			% Tail-recursive:
			loop(NewState);

		{command,y_request} ->
			NewState = apply_Y(State),
			loop(NewState);

		stop ->
			done;

		_Other ->
			NewState = apply_default(State),
			loop(NewState)

	end.


The start function of this template corresponds roughly to the WOOPER constructor, and then the WOOPER main loop manages the incoming calls to that object, updating step after step its state.



:raw-latex:`\pagebreak`

Methods
.......

Each class is able to define what are the methods that can be called on any of its instances. Calling a method of an instance is actually sending a message carrying the method name (as an atom), along with any parameter needed, to the process corresponding to that instance.

There are two kinds of methods:

 - *requests*, that return a result to the caller and may update the instance state
 - *oneways*, that do not return a result to the caller, i.e. that only trigger changes onto the instance state

WOOPER provides everything which is needed to manage appropriately the call of methods and the change in the instance state.

Therefore a class implementer just has to specify the class-specific elements, which are:

 - the mother class(es) of this class (if any)
 - the constructor(s)
 - any destructor needed
 - all class methods that:

   - are not inherited
   - or that would be inherited, but actually are overridden in this class

Note that, when a method is called, the current state of the instance (its ``State`` table) is always appended as first method parameter, therefore the method arity is always the number of parameters it expects plus one: a method named ``say_hello`` to be called with two parameters (ex: ``Instance ! {say_hello,[X,Y]}``) will be declared (exported) as ``say_hello/3``: ``say_hello( State, X, Y ) -> [..].``.

See, in the ``wooper/examples`` directory, the ``class_Cat.erl`` and ``class_Cat_test.erl`` files for method definition and calling.




:raw-latex:`\pagebreak`

Instance State
..............

WOOPER keeps track automatically of the instance state (which always remains purely private to its process), starting from the instance construction to its deletion, and in-between from one method call to another.

Inside a method body, WOOPER provides some constructs to manage the state of instances, which is an associative table, a collection of attribute pairs (attribute name/attribute value).

See, in the ``wooper/examples`` directory, the ``class_Cat.erl`` file for state management in method body.

In terms of inner workings, for faster look-ups, WOOPER 0.x and 1.x versions rely onan implementation of associative tables, the hashtable, based on the ``hashtable`` module we previously investigated.


Regardless of the WOOPER version, two associate tables are used.

There is first a per-class associative table, shared among all class instances, which acts exactly as the C++ virtual table: dynamically computed at program start-up, for all instances of a given class, it can directly convert a method name into an actual function to call, instead of having to perform a lengthy look-up through the inheritance hierarchy for that class.

There is also a per-instance associative table, which stores the state of a given instance.



.. admonition:: Interactive explanation: WOOPER tables

  A drawing made on the whiteboard will explain the role of both associative tables, in the case of a class Cat inheriting from class Mammal, reusing the ``setAge/2`` method as is, possibly overriding the ``getFurColor/1`` method and defining a new non-inherited method ``getWhiskerColor/1``.

  Explanations will then rely on the review of ``class_Mammal.erl``, ``class_Cat.erl`` and ``class_Cat_test.erl``. This latter test will be run, and state and virtual table will be interpreted.



Note also that in the context of Sim-Diasca, most inter-actor communications will be done based on specific constructs, so that a detailed understanding of WOOPER is not strictly necessary: Sim-Diasca relies on WOOPER, but the technical details are hidden and specific: simpler - but more constrained - primitives are to be used instead, and they are transparently converted by Sim-Diasca in WOOPER parlance.

However the mapping from the WOOPER constructs to the Erlang ones are interesting to be understood, in order to make the best use of Sim-Diasca.

Lots of improvements on WOOPER are possible, in terms of performances as well as in terms of memory footprint. Most of them are identified, and the most significant ones are to be performed.






:raw-latex:`\pagebreak`

WOOPER Walkthrough
------------------

The corresponding file-system layout is::

  .
  |-- GNUmakefile
  |-- GNUmakerules.inc
  |-- GNUmakesettings.inc
  |-- GNUmakevars.inc
  |-- doc
  |   |-- GNUmakefile
  |   |-- README.txt
  |   |-- wooper-example.dia
  |   |-- wooper-example.png
  |   `-- wooper.rst
  |-- examples
  |   |-- GNUmakefile
  |   |-- class_Cat.erl
  |   |-- class_Cat_test.erl
  |   |-- class_Creature.erl
  |   |-- class_Creature_test.erl
  |   |-- class_Mammal.erl
  |   |-- class_Mammal_test.erl
  |   |-- class_OvoviviparousBeing.erl
  |   |-- class_OvoviviparousBeing_test.erl
  |   |-- class_Platypus.erl
  |   |-- class_Platypus_test.erl
  |   |-- class_Reptile.erl
  |   |-- class_Reptile_test.erl
  |   |-- class_Template.erl.sample
  |   |-- class_Template_test.erl.sample
  |   |-- class_ViviparousBeing.erl
  |   `-- class_ViviparousBeing_test.erl
  |-- src
  |   |-- GNUmakefile
  |   |-- class_Wooper_attribute_test.erl
  |   |-- wooper.hrl
  |   |-- wooper_class_manager.erl
  |   |-- wooper_class_manager.hrl
  |   `-- wooper_class_manager_test.erl
  `-- top-GNUmakefile-for-releases


A deeper look to selected parts of following files, in the ``src`` directory, could be done:

 - ``wooper_class_manager.erl``: ``start/1``, ``loop/1``
 - ``wooper.hrl``: ``state_holder`` record, ``new`` operators, example: ``remote_synchronous_timed_new_link``, ``wooper_construct_and_run_synchronous``, the ``setAttribute`` macro, ``wooper_main_loop/1``
 - ``class_Wooper_attribute_test.erl``






:raw-latex:`\pagebreak`

Examples
--------

Check that the ``common`` layer is up-to-date, then go in the ``wooper`` directory and rebuild everything (``make clean all``).

Then run the test suite: still from the ``wooper`` directory, run: ``make test``.

The review of the ``class_Cat_test.erl`` script should help understanding as well how WOOPER should be used on the caller side.

Then a more thorough inspection of ``class_Cat.erl`` should be done.






:raw-latex:`\pagebreak`

Exercise
--------


.. admonition:: Exercise ex_wooper

  Add a new class modelling a pink flamingo [#]_, and test it.

  A pink flamingo is a viviparous being whose brood has on average 1.7 flamingo.
  All pink flamingos have a name, their feather color is obviously pink, and their height, expressed in centimetres, is a floating point value that can be specified at birth.

  The one specific task a pink flamingo can be told to perform is to filter for plankton, in one of these two locations: Camargue or Chile. Due to the feeding, in the former location its height would then increase of 2.5 cm, whereas in the latter case it would increase of 1 cm.

  No special answer from the pink flamingo is expected once it finished its filtering, although it could gobble a bit in the course of the operation.

  Hint: use the templates in the ``wooper/examples/class_Template*.erl.sample`` files, and get inspiration from the WOOPER documentation and from other examples, like ``class_Cat``.


.. [#] *Flamand rose*, in French.

.. comment: The corrected versions (``sim-diasca/doc/training/class_PinkFlamingo.erl`` and ``sim-diasca/doc/training/class_PinkFlamingo_test.erl``) can be directly copied, compiled and run from the WOOPER examples (``wooper/examples``).



Note: we are just currently at the level of a rather advanced distributed `multi-agent system <http://en.wikipedia.org/wiki/Multi-agent_system>`_, however this is **not** a simulation yet! No notion of simulation time (a fortiori no simulation time *uncoupled* from user time), no specific scheduling, no reordering of messages, no reproducibility: a lot of properties are not guaranteed and generally are not preserved.


This will done with Sim-Diasca, which is fully based on WOOPER but adds its own more advanced constructs and corresponding constraints.

:raw-latex:`\pagebreak`














:raw-latex:`\pagebreak`


Sim-Diasca
==========

The Sim-Diasca layer is the third of a simulator architecture, just on top of the WOOPER layer.

Is there any attendee who has not read the Sim-Diasca technical overview yet?


Installation
------------

As the simulation engine will be regularly improved with bug fixes, new features and performance enhancements, newer Sim-Diasca versions will have to be installed regularly by projects making use of it, so users have to be familiar with the installation procedure.



.. admonition:: Exercise ex_sim_diasca_install

  Refer to the ``SimDiasca-technical-overview-english.pdf`` guide, section 11.

  Installation is to be performed from the ``SimDiasca-training-install.tar.bz2`` archive.

  This just involves the LogMX installation (for the sake of this training, the evaluation version can be used), the checking that the Sim-Diasca parser suffers from no Java incompatibility, and the compilation of Sim-Diasca itself.

  From the root of the install, everything should rebuild flawlessly (``make clean all``).

  As a few model tests have not been updated regarding that install, ``make test`` in supposed to work only in the ``sim-diasca/core`` directory.






:raw-latex:`\pagebreak`

Basic Principles
----------------

To fulfil the simulation objectives (see the `Simulation Objectives`_ section), following technical choices have been made in the design of Sim-Diasca:

 - simulation time must be totally uncoupled from user time
 - causality must be ensured
 - the execution of a simulation should be parallel and distributed (concurrency is key to scalability)
 - the algorithm for the time management of simulation should itself be compatible with a very large number of instances

We will see below why these principles are important, by describing what would happen if they were not respected.




:raw-latex:`\pagebreak`

What if...?
-----------


If simulation time was directly user time?
..........................................

Then we could not go faster or slower than reality.

Sim-Diasca is able to run in interactive mode (which corresponds to simulation time being on par with user time), but as long as there is no operator during simulation (i.e. humans are not in the loop) and that no real device is to interact with the simulation, then the batch mode (i.e. run as fast as you can, regardless of actual time) is the most interesting mode.

Moreover, to run in interactive mode, enough computing resources have to be available so that the simulator is able at all to run faster than real time.






:raw-latex:`\pagebreak`

If simulation time was not totally uncoupled from user time?
............................................................

Then a simulation would produce different results depending on the available resources (ex: what computers it would run on).

Example: a deployment policy installing meters in a loop which would be unaware of simulation time (i.e. without a proper scheduling nor being integrated with that scheduling, thus being scheduled in user time); for example all other processes running on the computer would affect the simulation outcomes, which of course is not wanted.






:raw-latex:`\pagebreak`

If no specific mechanism was used to ensure simulation correctness in a parallel or distributed context?
............................................................

Then consequences could happen *before* their causes:

:raw-html:`<img src=causality-issues-english.png></img>`
:raw-latex:`\includegraphics[scale=0.75]{causality-issues-english.png}`







:raw-latex:`\pagebreak`

If no specific mechanism was used to ensure **reproducibility** in a parallel or distributed context?
............................................................

Then by default running twice the same simulation would generally produce different results, as concurrent events, unlike causality-related events, can happen in any order:

:raw-html:`<img src=reproducibility-issues-english.png></img>`
:raw-latex:`\includegraphics[scale=0.75]{reproducibility-issues-english.png}`

Analysis and comparison of results would not be reliable nor practical.

Moreover these always changing results would have no statistical significance, and some perfectly possible outcomes may *never* be shown by the simulator, which of course is not wanted.






:raw-latex:`\pagebreak`

If no specific mechanism was used to ensure **ergodicity** in a parallel or distributed context?
............................................................

Then, if only reproducibility was ensured, for any simulation scenario we would only see one trajectory of the system, whereas, based on the models and on the initial state, usually a very large number of different licit trajectories exists.

Disabling the message reordering for reproducibility would not solve the problem: the order induced by the technical layers would then prevail, whereas it is unable to offer any fairness regarding the possible outcomes.

Therefore specific mechanisms have to be implemented so that *every possible combination of concurrent events may actually happen*, and so that all counterpart combinations *have the same probability* of showing up in the simulations.



:raw-latex:`\pagebreak`

As a consequence:

 - unless adequate simulation-specific tools are used (ex: if just using OOP languages or even multi-agent frameworks, which includes Erlang and WOOPER), then none of these properties is expected to be preserved
 - hence the need of specific constructs, associated with specific constraints to respect
 - the purpose of Sim-Diasca is to support these expected properties
 - what is difficult is to ensure them *in a concurrent context*, which is key to scalability






:raw-latex:`\pagebreak`

Sim-Diasca Mode Of Operation
----------------------------


Simple Case
...........

This describes the first versions of Sim-Diasca, i.e. all versions tagged 0.x or 1.x. The 2.x versions introduced more complex features, notably a distributed mode of operation and an advanced scheduling system for models, however these can be seen as a generalization of the "simpler" mode of operation explained here.

So the underlying technical mechanisms have been introduced in order to preserve the aforementioned properties; the good news is that this can be achieved relatively easily.



Basic principles
________________

 - simulation time is a purely virtual time
 - time-stepped simulations:

	- simulation time is chopped into ticks, of constant duration in virtual time
	- a fundamental simulation frequency can therefore be defined (ex: 50 Hz means 20 ms ticks)

 - simulation time is controlled by a (root) time manager, which schedules all simulation actors (notably it notifies them of the beginning of each tick)

 - a simulation consists on interacting actors, which communicate between them only thanks to *actor messages*


:raw-latex:`\pagebreak`

As shown in the diagram:

:raw-html:`<img src=tick-timescale-english.png></img>`
:raw-latex:`\includegraphics[scale=0.8]{tick-timescale-english.png}`

 - an *actor message* M is an asynchronous message sent from an actor A to an actor B at a tick T, which must acknowledge it at once (but this message is only *stored* by B, which does not process it during T)

 - actor A must wait until having received all acknowledgements for all the actor messages it sent at tick T (including M), before finishing its own tick

 - when all actors have finished their tick T, then the time manager notifies them immediately that tick T+1 has begun

 - when an actor is notified that a new tick begins:

  #. it first reorders all the actor messages (if any) it received on the previous tick, then it process them according to that new order, one after the other
  #. secondly that actor develops its spontaneous behaviour, it acts

 - for an actor, processing an actor message or acting is just a matter of changing its own state and/or sending actor messages to other actors

 - an actor that determines at tick T that it must be removed from the simulation will stop being scheduled after the next tick


For 2.x versions, distribution is transparent to model instances: like before, they are able to interact with each other and with a local time manager, on their node. The engine, thanks to a scheduling hierarchy of time managers, is able to control the global virtual time and all the interactions across the set of computing nodes, in an efficient way.

Advanced scheduling basically uncouples the processing of past actor messages from the spontaneous behaviour of an actor, and allows actors to plan at will their next spontaneous action, which may happen at any future tick. As a result, for each actor, a given simulation (overall) tick is either:

 - a *triggered* tick: there is at least one actor message, from the previous simulation tick, to process, and no spontaneous behaviour to develop

 - a *spontaneous* tick: reciprocally, there is no actor message here to process, only a spontaneous action to execute

 - a *two-fold* tick: there are both at least an actor message and a spontaneous action to process here

 - a tick which is not scheduled at all for that actor, neither for actor message nor for a planned action


As a result, as soon as for a given simulation (overall) tick no actor is to be scheduled, this tick can safely be skipped, and the engine actually will automatically jump instantly over all idle durations (i.e. simulated periods during which no event can possibly happen), which is especially interesting for systems which may register no activity during some periods (ex: tremendous acceleration if a 200Hz simulation involves event bursts separated by, say, centuries).




:raw-latex:`\pagebreak`

Consequences
____________

 - ticks do not last more than strictly necessary:

	- a tick ends exactly at the moment when the last actor reports having finished it
	- the next (scheduled) overall tick follows then immediately (no waiting induced at all, in batch mode)

 - a key point: each actor can process its tick *completely independently from all other actors*; therefore all actors, how numerous they may be, can be evaluated fully concurrently, i.e. in parallel and/or in a distributed way

  - for a tick, the actual duration is the maximal duration in all the actors, instead of the sum of all actor durations

  - ex: 35 million actors evaluated in parallel, rather than one after the other

 - two simulation modes:

  - batch: best effort, the simulator runs as fast as possible
  - interactive: the simulator, supposedly having access to enough computing resources to be able to run faster than real time, slows down as much as needed to keep exactly on par with user time

 - to ease troubleshooting:

  - a watchdog is automatically triggered in batch mode
  - application-level deadlocks can be identified: faulty models are detected at runtime



.. admonition:: Exercise ex_algorithm

  Explain in a few words why all the expected simulation properties are met with this algorithm.

  Justify why each message involved in the algorithm is strictly necessary.

  Hint: use proof by contradiction, by finding an example where, if such a message did not exist, at least one property could not be guaranteed in same cases.




:raw-latex:`\pagebreak`

In practise
___________


The **model implementer**:

 - does *not* have to take care explicitly of these rather complex message patterns related to scheduling and acknowlegment, as they are automatically managed by Sim-Diasca

 - just has to concentrate on the actor specific spontaneous behaviour (the ``actSpontaneous/1`` method) and the triggered behaviours it may support (i.e. the various methods it may declare)



In terms of classes, there are:

 - the **time manager** (``class_TimeManager``): sole responsible for the simulation scheduling
 - the **actor actual classes**:

  - which are instances of simulation models
  - which are (direct or not) child classes of the ``class_Actor`` overall mother class
  - whose instances manage their state exactly as done with vanilla WOOPER (no difference)



Simulation actors generally have to **interact**. To do so:

 - actors must not use direct WOOPER requests, which take into account neither the simulation time nor the expected simulation properties

 - actors have to use instead *actor messages*



An **actor message**:

  - can be seen as a oneway asynchronous message sent by an actor to another

  - is to be sent thanks to the ``class_Actor:send_actor_message/3`` function: for example, instead of ``AnActorPid ! {a_method,Params}``, a simulation actor should use::

	 NewState = class_Actor:send_actor_message( AnActorPid, {a_method,Params}, State )


  - is actually an automatically wrapped, specifically-managed WOOPER oneway call, reordered and processed after the simulation tick progressed


With actor messages:

  - any request (a notification implying an answer sent back to the caller) can be implemented as the exchange of two actor messages (notification from caller to callee, then answer from callee to caller; with a corollary: a two-tick latency)

  - during a given tick, actor messages are received by a recipient in any order (as dictated by the technical context), but that recipient will reorder them automatically on the next tick:

   - in reproducible mode: actor messages are sorted according to an arbitrary order which depends only on the messages themselves (their content) and on their sender

   - in ergodic mode: actor messages are further permuted according to a random uniform law, based on an appropriate seeding, so that all combinations can be recreated (moreover with the same probability)




:raw-latex:`\pagebreak`

The actor **life-cycle**:

 - comes to an end when the actor ``triggerTermination/1`` method is called

 - is managed based on continuation (``done``) and termination (``terminated``) messages

 - a terminating actor will automatically unregister from the scheduler and perform its deletion



An actor that depends on random laws is a **stochastic actor** [#]_: it can make use of any number of stochastic variables respecting any probability density function(s).



.. [#]

  Previously such an actor was to inherit from ``class_StochasticActor`` and to declare at creation time the stochastic variables it needed, like the four lists defined in:

  ``[ { my_first_uniform,{uniform, 5}}, {my_second_uniform,{uniform,100}}, {my_gaussian,{gaussian,50,2}}, {my_exponential,{exponential,80}} ]``

  That prior system to manage stochastic values was complex, induced constraints and required a mechanism to manage a graph of actor waiting for others; luckly the newer implementation is both a lot simpler, more efficient and does not have to rely on any waiting mechanism, nor on a specific mother class, and all actors can be stochastic at virtually no cost.



A rather advanced **distributed trace system** is also available.




.. admonition:: Interactive explanation: reordering of events

  Causality-related events have to happen in a strict, fixed, order.

  According to the point of view of any given actor, at any tick, *all* actor messages to be processed are truly concurrent events.

  Example of a meter actor receiving messages M1, M2 and M3, with:

	- M1 being a tariff setting request
	- M2 being a tariff reading request
	- M3 being a meter shut-down command

  Case #1: no specific order is enforced, most of the time we may receive [M2,M1,M3], sometimes [M2,M3,M1], maybe other combinations never happen. Neither reproducible nor ergodic, unreliable.

  Case #2: reproducibility is requested, at each run with constant initial conditions for the simulation, we happen to receive for this trajectory *always* [M2,M1,M3], for example.

  Case #3: ergodicity is requested, on average (after a large number of runs), all 6 possible message permutations occur statistically at the same rate (16,7%).

  See ``class_Actor:apply_reordering/2`` if interested by the actual implementation.

:raw-latex:`\pagebreak`




.. comment: :raw-html:`<img src=PLCCommunicationModel-tick-0.png></img>`
.. comment: :raw-latex:`\includegraphics[scale=0.4]{PLCCommunicationModel-tick-0.png}`

:raw-latex:`\pagebreak`


.. comment: :raw-html:`<img src=deterministic-test-instances.png></img>`
.. comment: :raw-latex:`\includegraphics[scale=0.75]{deterministic-test-instances.png}`

:raw-latex:`\pagebreak`





:raw-latex:`\pagebreak`

Sim-Diasca Walkthrough
----------------------

By overall feature:

 - deployment management: ``class_DeploymentManager``, ``class_ComputingHostManager``, ``deployment_agent``

 - scheduling management: ``class_TimeManager``, ``class_Actor``, ``class_LoadBalancer``

 - random management: ``class_RandomManager``

 - result management: ``class_ResultManager``, ``class_ResultProducer``

 - data management: ``class_DataLogger``, ``class_Probe``, ``class_DataExchanger``

 - trace management: ``class_TraceEmitter``, ``class_TraceListener``, ``class_TraceAggregator``, ``class_TraceSupervisor``

 - performance management: ``class_PerformanceTracker``


.. comment:
 - simulation management: ``class_SimulationManager``, ``class_ScenarioManager``




Review of other helper modules:

 - generic services: ``class_Describable``, ``class_Graphable``

 - graph services: ``class_Mesh``

 - other services are very minor





:raw-latex:`\pagebreak`

Sim-Diasca Exercises
--------------------

First exercise is about model design, from modelling to implementing a simple model, based on textual requirements.



.. admonition:: Exercise ex_sim_diasca_design

  Return of the Pink Flamingo.

  Turn the class defined in the WOOPER section into a simulation actor (``class_VilifyingPinkFlamingo``) with following additional traits: each pink flamingo lives in Camargue, may be introduced to a rival flamingo, in which case from time to time the first flamingo will shamelessly vilify [#]_ its rival, about how short-legged it supposedly is.

  When not being busy vilifying this rival, that flamingo will filter plankton to become soon taller and taller than...you know who.

  However, if having no rival, then the flamingo will just mumble about how sweet its life is.

  Finally, when any flamingo is vilified by another, then the former will in turn see the latter as a rival, regardless of any previous rivalry (no flamingo can afford to have more than one rival at any time).

  This design exercise involves:

	- doing a bit of (flamingo) modelling
	- modifying the inheritance and construction of the pink flamingo: it must become a simulation actor
	- defining what stimuli it can receive from the environment:

	 - it must be able to be notified of the existence of any initial rival, so that it can vilify it
	 - reciprocally it must become aware that it became the target of any vilifying in progress, so that it can react accordingly

	- defining what is its spontaneous behaviour: from time to time (say, one tick every three ticks), either vilifying any rival or filtering plankton, otherwise just mumbling (through the trace system)

  See the ``class_VilifyingPinkFlamingo_test.erl`` test case to create a compatible flamingo.
  Copy this file from the training extracted archive in your ``sim-diasca/src/core/src`` directory, copy there also ``wooper/examples/class_ViviparousBeing.beam`` (as these flamingos are still viviparous beings) in the same directory, and create your flamingo: write, compile, run, test, and back, until respecting the specification.



.. [#] In French: *vilipender*, *médire*.



:raw-latex:`\pagebreak`

Second exercise is operating on a slightly more elaborate set of models, which are faulty by design, to learn how to fix bugs and overcome mistakes.


.. admonition:: Exercise ex_sim_diasca_model_troubleshooting

  Hunt and fix the faulty models.

  Some vandals, members of a pro-dietetics underground group, apparently messed with the soda vending machine and thirsty customer test case, and observers reported that it does not seem to work properly anymore.

  You have been called in emergency so that this hopeless situation is solved quickly.
  Will you be able to repair these models on time, i.e. before the end of the training?

  Go into the training extracted archive and copy:

	- ``class_FaultyDeterministicThirstyCustomer.erl``
	- ``class_FaultySodaVendingMachine.erl``
	- ``soda_deterministic_faulty_integration_test.erl``
  in your ``sim-diasca/src/core`` directory, and eliminate all bugs in the models that make the simulation crash or be incorrect: at the end, ``make soda_deterministic_faulty_integration_run`` should run not only flawlessly but also as expected from the implied specification (the way things work in reality).

  Hint: for these two models, there are a total of at least 6 mistakes, with at least two errors involving each layer (Erlang, WOOPER, Sim-Diasca).






:raw-latex:`\pagebreak`

Advanced Case
.............

This describes more specifically how Sim-Diasca is to be used in a distributed context.


:raw-html:`<img src=SimDiasca-physical-dispatching-english.png></img>`
:raw-latex:`\includegraphics[scale=0.27]{SimDiasca-physical-dispatching-english.png}`

.. comment:

 :raw-html:`<img src=SimDiasca-placement-scheduling-sequence.png></img>`
 :raw-latex:`\includegraphics[scale=0.25]{SimDiasca-placement-scheduling-sequence.png}`



Key elements:

 - a scheduling hierarchy based on a distributed time management allowing for flexible granularity (ex: at the core/processor/computer/cluster level), based on time managers: they drive actors, directly or through child managers, forming a scheduling tree, whose root controls and monitors the overall simulation time and interacts with the user

 - advanced actor scheduling: hybrid engine, time-stepped but with some event-based features (jumping directly over idle periods)

 - use of a load balancer:

	- automating the placement: no more hard-coded actor locations
	- more efficient and flexible actor dispatching: starting with a round-robin (static) policy, advanced placement policies could be imagined (even dynamic ones)
	- solving the actor identifier issues; by-product: possibility of actor migration


As a consequence, the API as seen by a model instance requires it:

 - to tell its time manager about the next tick, if any, at which it should be scheduled for action (spontaneous behaviour); for example an actor can remain purely passive, adopt a periodical behaviour, or apply a step-by-step arbitrary scheduling policy

 - to handle being scheduled in response to an actor message sent (i.e. its corresponding methods will thus be called), although it had not specifically planned to be triggered this tick (processing of actor messages and spontaneous behaviour can happen separately)

 - an actor, once it received an unplanned actor message, might withdraw its next initially planned tick, and reschedule it at will




:raw-latex:`\pagebreak`

Modelling
---------

See the ``Sim-Diasca Modelling Guide``, section 10 of the ``SimDiasca-technical-overview-english.pdf`` document.

Open discussion about how these guidelines could be applied to the specific context of use of Sim-Diasca, for the simulation target.


.. comment :raw-html:`<img src=modelling-approach-english.png></img>`
.. comment :raw-latex:`\includegraphics[scale=0.75]{modelling-approach-english.png}`




:raw-latex:`\pagebreak`

Mock Simulators
===============

The *Mock Simulators* repository gathers a set of in-depth examples of use of the Sim-Diasca engine, to provide simplistic yet complete toy simulators, to help developers getting started.

There are three main examples of interest:

 - the *Soda Test* case, which implements a tiny simulation of soda-vending machines interacting with deterministic or stochastic thristy customers

 - the *SSI-Test* case, which stands for *Sim-Diasca Scalable Integration Test*, a more comprehensive, business-free simulation case, which allows notably to check various simulation properties and performances

 - the *City-Example* case, which is geared towards the benchmarking of larger, longer simulations, in the context of another business-free simulation case, dealing with urban concerns; it is designed to be easily tunable in terms of scalability



:raw-latex:`\pagebreak`




Annexes
=======



Source Control System
---------------------

 - based on SVN (Subversion), see `its official manual <http://svnbook.red-bean.com/en/1.5/svn-book.html>`_ (one may prefer the *Single-page HTML edition*)

 - version 1.5 or higher required (otherwise merge-reintegrate will not be properly supported)

 - some conventions about the use of SVN have to be retained, this includes: user and branch naming, merging policies, tagging, releasing; these conventions, and others, are documented in the *Sim-Diasca Developer Guide*


:raw-latex:`\pagebreak`

The layout of the sources [#]_ is the following::

  |-- common
  |   |-- conf
  |   |-- contrib
  |   |   `-- smart-exceptions
  |   |-- doc
  |   `-- src
  |       |-- data-management
  |       |-- maths
  |       |-- scripts
  |       |-- user-interface
  |       `-- utils
  |-- mock-simulators
  |   |-- doc
  |   |-- soda-test
  |   |   |-- doc
  |   |   `-- src
  |   `-- ssi-test
  |       |-- doc
  |       `-- src
  |-- sim-diasca
  |   |-- conf
  |   |   `-- clusters
  |   |-- doc
  |   |   |-- building-blocks
  |   |   |-- licence
  |   |   |-- training
  |   |   `-- xkcd.com
  |   `-- src
  |       |-- core
  |       `-- models
  |-- traces
  |   |-- conf
  |   |   `-- logmx
  |   |-- doc
  |   `-- src
  `-- wooper
	  |-- doc
	  |-- examples
	  `-- src



.. [#] Obtained with: ``tree . -L 3 -d``.






:raw-latex:`\pagebreak`


Documentation System
--------------------

We use `RST <http://docutils.sourceforge.net/rst.html>`_ (Restructured Text, which is a *Mark-up Syntax and Parser Component of Docutils*) as syntax for most of our documentation [#]_.

We also make use of `Trac <http://trac.edgewall.org/>`_, as a wiki, as it can understand the same syntax.

`Example of RST page source <http://cln46mr.der.edf.fr:8000/Sim-Diasca/wiki/ErlangIde>`_ (only available from the EDF network).

.. [#] Note that docutils packages more recent than ``python-docutils_0.4-6_all.deb``) seem to have a formatting bug for PDF generation.



.. comment :
  Bug-Tracking System
  -------------------

  As soon as some people are working collaboratively on common developments, some tools beyond the source control system can help a lot, and this includes bug-trackers.

  A well-known open source bug-tracker is `Mantis <http://www.mantisbt.org/>`_.



  Mailing Lists
  -------------

  They will surely be needed.
