==========================================================
Correction of the Exercises of the ``Sim-Diasca`` Training
==========================================================


+---------------------------------+---------------------------------+
| .. image:: sim-diasca.png                                         |
|   :scale: 30                                                      |
|   :align: center                                                  |
+=================================+=================================+
| .. image:: logo-EDF-english.png | .. image:: lgplv3-147x51.png    |
|   :scale: 50                    |   :scale: 20                    |
|   :align: left                  |   :align: center                |
+---------------------------------+---------------------------------+


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Author: Olivier Boudeville
:Contact: olivier.boudeville@edf.fr
:Organisation: Copyright (C) 2008-2013 EDF R&D
:Creation Date: January 2010
:Last Update: Monday, March 4, 2013
:Status: Work in progress
:Version: 0.4
:Dedication:

	For people attending to the Sim-Diasca training.


:Abstract:

	Here are presented all the corrected versions of the exercises to be made in the course of the Sim-Diasca training.



.. meta::
   :keywords: Sim-Diasca, massive, simulation, multi-agent, development, training, corrected, exercises


:raw-latex:`\pagebreak`

.. contents:: Sim-Diasca Corrected Exercises
	:depth: 2

.. section-numbering::



:raw-latex:`\pagebreak`

Exercise ex_erlang_install
==========================

When running the interpreter, something similar to::

  Erlang R16B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

  Eshell V5.10.1  (abort with ^G)

should be output.

With nedit, WOOPER keywords should be highlighted, as Erlang ones are.




:raw-latex:`\pagebreak`

Exercise ex_make
================

Every target should be correctly executed::

  > make clean all test
	   Cleaning all in $PREFIX/common
	   Cleaning all in $PREFIX/common/src
	   [..]
  --> End of test for module net_utils.
	 Building all in $PREFIX/common/doc
	 Testing all in doc




:raw-latex:`\pagebreak`

Exercise ex_compiler
====================

Output should be like::

 > make clean hashtable.beam
	Cleaning all in $PREFIX/common/src
	 Compiling module hashtable.erl
  `PATH=~/Software/Erlang/Erlang-current-install/bin:$PATH which erlc` -b beam -I. -I../src -v -W2 +debug_info -o hashtable.beam hashtable.erl

Options are:

   - ``-b`` type: type of output file (e.g. jam or beam)
   - ``-I``: designates paths that should be scanned when looking for ``*.hrl`` include files
   - ``-v``: verbose compiler output
   - ``-Wnumber``: set warning level to number
   - ``+term``: pass the Erlang term unchanged to the compiler; ``debug_info`` means *Include debug information in the form of abstract code (see The Abstract Format in ERTS User's Guide) in the compiled beam module. Tools such as Debugger, Xref and Cover require the debug information to be included*
   - ``-o name``: name output directory or file

See for more details:

 - ``erlc -h``
 - the doc regarding the ``erlc`` and ``compile`` modules




:raw-latex:`\pagebreak`

Exercise ex_interpreter
=======================

We should have::

  > make hashtable_run
	   Compiling module hashtable_test.erl
	   Running unitary test hashtable_run (second form) from hashtable_test hashtable.beam
  ../src/launch-erl.sh -v -c Ceylan-common --ln ceylan_test --fqdn `hostname -f` --beam-dir . --beam-dir ../src --ln hashtable_run  --

	+ Bucket with 1 element(s):
	   * MyFirstKey -> MyFirstValue
  --> End of test for module hashtable.


Change in ``hashtable_test.erl`` ``{value,"MyFirstValue"}`` by ``{value,"AWrongValue"}``, rerun the test::

  > make hashtable_run
	   Compiling module hashtable_test.erl
	   Running unitary test hashtable_run (second form) from hashtable_test hashtable.beam
  ../src/launch-erl.sh -v -c Ceylan-common --ln ceylan_test --fqdn `hostname -f` --beam-dir . --beam-dir ../src --ln hashtable_run  --
  [..]
  + Bucket with 1 element(s):
	 * MyFirstKey -> MyFirstValue

   Looking up for MyFirstKey: {value,"MyFirstValue"}
   {"init terminating in do_boot",{{badmatch,{value,"MyFirstValue"}},[{hashtable_test,run,0},{erl_eval,do_apply,5},{init,start_it,1},{init,start_em,1}]}}

   Crash dump was written to: erl_crash.dump
   init terminating in do_boot ()
   make: *** [hashtable_run] Error 1


Command line is::

   Running unitary test hashtable_run (second form) from hashtable_test hashtable.beam
  ../src/launch-erl.sh -v -c Ceylan-common --ln ceylan_test --fqdn `hostname -f` --beam-dir . --beam-dir ../src --ln hashtable_run  --eval `echo hashtable_run | sed 's|_run|_test:run()|1'`
  Launching: erl +W w -pz  . ../src -smp auto +K true +A 8 +P 120000  -setcookie Ceylan-common -eval hashtable_test:run()  -name hashtable_run

Options for the ``erl`` interpreter are:

 - ``+W w``: messages sent to the error logger are mapped to warnings
 - ``-pz . ../src``: adds the specified directories to the end of the code path
 - ``-smp auto``: starts the Erlang runtime system with SMP support enabled if it is available and more than one logical processor are detected
 - ``+K true``: enables the kernel poll functionality if the emulator supports it
 - ``+A 8``: sets the number of threads in async thread pool to 8, valid range is 0-1024. Default is 0
 - ``+P 120000``: sets the maximum number of concurrent processes for this system. Number must be in the range 16..134217727. Default is 32768.
 - ``-setcookie Ceylan-common``: sets the magic cookie of the node to ``Ceylan-common`` (as ``common`` is a part of Ceylan that can be used without WOOPER; cookies are renamed by each upper layer)
 - ``-eval hashtable_test:run()``: makes init evaluate the specified expression
 - ``-name hashtable_run``: makes the Erlang runtime system into a distributed node. This flag invokes all network servers necessary for a node to become distributed.It is also ensured that epmd runs on the current host before Erlang is started. The name of the node will be hashtable_run@Host, where Host is the fully qualified host name of the current host


See for more details:

 - ``man erl``
 - the doc regarding the ``erl`` module





:raw-latex:`\pagebreak`

Exercise ex_monitor
===================

This is an example of an anonymous function which is (tail-)recursive thanks to a Y-combinator.

It keeps on calling itself, so it is expected to fully load any core it is running on.

So the first code snippet should load exactly one core, at its maximum capacity.

The second snippet is based on a list comprehension: the same infinitely recursive function is called as many times as there are available cores.

As the VM settings in use select all available core, on a 8-core computer, 8 instances of the recursive function will be called. The (local) SMP-aware scheduler of the Erlang VM then will dispatch their processing on all the available cores.

Therefore one should see with the load monitor that all cores run now at their maximal load. This should the automatic parallelization allowed by Erlang.




:raw-latex:`\pagebreak`

Exercise ex_wooper
==================

 - translation: a PinkFlamingo class, inheriting from the ViviparousBeing class, must be created an tested
 - the PinkFlamingo class must:

  - override the ``getMeanChildrenCount/1`` method so that it returns 1.7
  - have an attribute whose name is ``name`` and whose value is ``Syd``; as it is constant, it does not need to be specified in the constructor
  - have an attribute whose name is ``feather_color`` and whose value is ``pink``; as it is constant, it does not need to be specified in the constructor
  - have an attribute whose name is ``height``, to be specified in the constructor
  - a ``filterPlankton/2`` oneway must be defined, taking, beyond the state, one parameter, the target location, which is either ``camargue`` or ``chile``;
  - this oneway should just output a message (with ``io:format``) like "Gobble, gobble"


Recommended actions::

	cd wooper/examples
	cp class_Template.erl.sample class_PinkFlamingo.erl
	nedit class_PinkFlamingo.erl
	cp class_Template_test.erl.sample class_PinkFlamingo_test.erl
	nedit class_PinkFlamingo_test.erl
	make class_PinkFlamingo.beam class_PinkFlamingo_test.beam
	make class_PinkFlamingo_run


Example of result::

  > make class_PinkFlamingo_run

	 Running unitary test class_PinkFlamingo_run (second form) from class_PinkFlamingo_test class_PinkFlamingo.beam
	 ../../common/src/launch-erl.sh -v -c WOOPER --ln wooper_test --fqdn `hostname -f` --beam-dir ../src --beam-dir . --beam-dir ../../common/src --ln class_PinkFlamingo_run  --eval `echo class_PinkFlamingo_run | sed 's|_run|_test:run()|1'`
	 Launching: erl +W w -pz  ../src . ../../common/src -smp auto +K true +A 8 +P 120000  -setcookie WOOPER -eval class_PinkFlamingo_test:run()  -name class_PinkFlamingo_run
	 Erlang R16B (erts-5.10.1) [source] [rq:1] [async-threads:8] [hipe] [kernel-poll:true]

	 Eshell V5.10.1  (abort with ^G)
	 (class_PinkFlamingo_run@localhost.localdomain)1> --> Testing module class_PinkFlamingo.
	 --> Debug mode: true.
	 --> Statically, class name is class_PinkFlamingo, superclasses are [class_ViviparousBeing].
	 --> After constructor, get_class_name returned 'class_PinkFlamingo' as expected.
	 --> After constructor, get_superclasses returned [class_ViviparousBeing] as expected.
	 --> On average a flamingo has 1.700000 children.
	 [Syd] Glouglou, gouglou, my height is now 122.500000 cm.
	 [Syd] Glouglou, gouglou, my height is now 125.000000 cm.
	 [Syd] Gobble, gobble, my height is now 126.000000 cm.
	 [Syd] Glouglou, gouglou, my height is now 128.500000 cm.
	 --> The flamingo is pink, as expected.
	 --> End of test for module class_PinkFlamingo.






:raw-latex:`\pagebreak`

Exercise ex_sim_diasca_install
==============================

Just follow the Sim-Diasca installation guide.

As the Sim-Diasca version used for the training is between two versions of the engine, not all tests may succeed.






:raw-latex:`\pagebreak`

Exercise ex_algorithm
=====================

Let's check together that indeed:

 - simulation time is completely uncoupled from user time
 - causality is necessarily respected
 - reproducibility can be ensured
 - ergodicity can be ensured


Regarding messages:

 - top *and* done messages are needed, as otherwise an actor would not know when a received actor message was sent at tick T or tick T+1 (note that it is perfectly licit for an actor to receive - and acknowledge - an actor message for tick T whereas it has already finished this tick T); for receiving actor, race condition between this message and the next ``top``

 - acknowledgement message

	- is *not* needed "because some message could be lost"
	- it is needed since it is the only way of forcing the overall tick to last as long as all actor messages have not finished being exchanged for sure, with no assumption about their propagation time (not bounded on a network)

 - sending ``done`` either immediately, if no actor message was sent, or after having waited for all actor message acknowledgements to have been received: otherwise an actor could receive a message from the past: already at tick T+1, it would discover it should have managed a message at tick T

 - actual termination of an actor must happen on the next tick only, as a terminated actor may still receive an actor message during its termination tick: it must be able to acknowledge it nevertheless






:raw-latex:`\pagebreak`

Exercise ex_sim_diasca_design
=============================

Hints:

 - the flamingo must inherit from ``class_Actor`` and its constructor must be updated
 - a method ``discoverRival(State,RivalFlamengoPid)`` could be defined
 - the ``actSpontaneous/1`` method could determine if the flamingo decides this tick to calumniate its rival and, if appropriate, send it

See the full sources of the correct models, provided with this correction.






:raw-latex:`\pagebreak`

Exercise ex_sim_diasca_model_troubleshooting
============================================

.. Note: Making a diff' with the correct test case is considered to be cheating!



Bugs to be removed are:

 - Erlang level:

  - in the machine code:

	- the module should be named ``class_FaultySodaVendingMachine``, not ``class_FlamingoSodaVendingMachine``
	- in the ``orderSoda`` oneway, the first clause of the pattern-matching always succeeds (clauses were reversed)

  - in the customer code, ``set_next_thirsty_tick`` is called with a wrong arity, the ``State`` variable should be specified

  - the ``repletion_duration/0`` type should be documented as a number of ticks, not seconds

  - the ``onNotEnoughMoney/2`` method should be decored with a type specification


 - WOOPER level:

  - in the vending machine code, the construction corresponding to the Actor mother class is lost, short of reusing the corresponding state (moreover the trace sending makes use of ``ActorState`` and thus may hide the warning)

  - in the customer code, the ``request_cost`` helper function attempts to call a non-existing method of the vending machine: ``getCostOfCan`` is used instead of ``getCanCost``

  - in the vending machine code, the ``orderSoda`` oneway tries to return a result as if it was a request (correct code is: ``?wooper_return_state_only( NewState )``)

 - Sim-Diasca level:

  - in the test case, the sending of a trace message when starting the time manager is incorrectly called, as the stop tick should be an integer, not a float (fix: ``StopTick = 30``)

  - in the vending machine code, in the ``getCanCost`` actor oneway, a direct (WOOPER) oneway call is made, instead of sending a (Sim-Diasca) actor message; correct code is: ``?wooper_return_state_only( class_Actor:send_actor_message( CustomerPid,{setCanCost,?getAttr(can_cost)}, State ) )``

  - in the code for the deterministic thirsty customer, for the ``actSpontaneous/1`` oneway:

	- the type specification  should mention a return type of ``oneway_return()`` rather than ``wooper_state()`` (both are true, but the former is more informative than the latter); note that, for actor oneways, it would have been ``class_Actor:actor_oneway_return()``

	- ``NewState`` should be returned instead of ``State``


 - in the test case:

	- synchronous creations of actors should be used, otherwise the time manager could have already reached its termination tick even before these actors have subscribed (race condition)

	- ``test_receive/0`` should be used instead of ``receive..end`` clauses in tests, lest unexpected message are intercepted
