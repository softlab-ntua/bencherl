:raw-latex:`\pagebreak`


--------------------------------
Sim-Diasca Implementation Review
--------------------------------

Let's suppose we want to simulate interacting models. Let's see how easy it is to use Sim-Diasca to do so, on a simplified yet representative simulation test case.


Deterministic Models
====================

In that section, simple models are implemented, and they do not need here stochastic variables.

For the sake of pedagogy, let's suppose that this test involves two models:

 - the model of a soda vending machine
 - the model of a thirsty customer


 Several instances of these models (i.e. several simulation actors) must be able to interact gracefully with respect to the simulation expected properties, and we want to see the outcome of the corresponding simulation, like the stock of cans that each machine holds over time.


For this test case, we want following basic scenario to be simulated:

 - there are two soda vending machines (SVM1 and SVM2)
 - there are three thirsty customers (each of them knowing the other two): TC1 and TC2, who will be both using SVM1, and TC3, who will use SVM2


:raw-html:`<img src="deterministic-test-instances.png"></img>`
:raw-latex:`\includegraphics[scale=0.4]{deterministic-test-instances.png}`



Unless mentioned otherwise, we consider that an interaction is to obey a specific timing.

For example, we can consider that communications (ex: a customer speaking to another) are instantaneous, perceptions (ex: a customer looking at a machine to read the cost of one of its cans, supposedly displayed on the machine) as well, but that other actions (ex: a machine processing a can order) last for some model-specific duration (ex: 800 ms of virtual time may elapse before a valid purchase results in a can being delivered to the corresponding customer).


Model of the Soda Vending Machine
---------------------------------

This model will be implemented in the ``SodaVendingMachine`` class, named ``class_SodaVendingMachine``, whose code will be stored in the ``class_SodaVendingMachine.erl`` file (``.erl`` is the standard extension for Erlang files).

Each of its instances will be **created** based on three parameters, telling for each machine:

	1. the name of the machine, to help understanding the simulation traces being produced
	2. how many soda cans there are in stock initially
	3. how much money a can of soda from that machine costs (expressed as an integer number of euros)


The **state** of a machine will be defined by largely similar variables [#]_:

+-------------------+------------+-----------------------------------+
| Variable Name     | Type       | Meaning                           |
+===================+============+===================================+
| ``name``          | String     | Name of the machine.              |
+-------------------+------------+-----------------------------------+
| ``can_count``     | Integer    | Number of cans in stock.          |
+-------------------+------------+-----------------------------------+
| ``can_cost``      | Integer    | Cost of a can for a customer.     |
+-------------------+------------+-----------------------------------+

.. [#] Note that this is the specification-level state of the machine. For technical and simulation-related reasons, the actual state may comprise additional instance-level attributes (ex: a trace identifier, a reference to a probe, etc.).

In terms of **behaviour**, a machine will sell soda on request (being mostly passive), until it runs out of cans. Any number of cans can be bought at any time by any customer, provided there are enough cans left in the machine and that the customer can afford it.



Model of the Deterministic Thirsty Customer
-------------------------------------------

Regarding **behaviour**, each instance of this customer (``class_DeterministicThirstyCustomer``) that happens to be thirsty would regularly (try to) purchase a can of soda from the (only) machine he knows, if that customer can still afford to buy a can from it.

Otherwise, this customer may decide to borrow money from any other customer he knows.

As the model must be deterministic here, the customer may try to borrow the money he still needs only if it has already an even number of euros in pockets (possibly zero), and then he would target only the first other customer he may know (if any), supposing he can obtain a list of them sorted in any reproducible order.

A customer who is asked for money will accept iff he has the requested amount of euros, if it is an odd number, and if it is not already involved in any other interaction.


Each customer will be **created** based on five parameters, telling:

	1. the name of this customer, to help understanding the simulation traces being produced

	2. what is the vending machine he can use

	3. what are the other customers he knows of

	4. how much time will elapse before being thirsty again, once having drunk a can (expressed as an integer number of minutes)

	5. what is the total amount of money this customer has initially in pocket, and therefore can spend (expressed as an integer number of euros)



The **state** of a thirsty customer will be defined by largely similar variables:

+-----------------------+------------+-----------------------------------+
| Variable Name         | Type       | Meaning                           |
+=======================+============+===================================+
| ``name``              | String     | Name of the customer.             |
+-----------------------+------------+-----------------------------------+
| ``known_machine``     | Identifier | Identifier of the machine this    |
|                       |            | customer knows.                   |
+-----------------------+------------+-----------------------------------+
| ``known_customers``   | List of    | Identifiers of the customers this |
|                       | Identifiers| customer knows.                   |
+-----------------------+------------+-----------------------------------+
| ``can_cost``          | Integer    | Cost of a can from this machine.  |
+-----------------------+------------+-----------------------------------+
| ``repletion_duration``| Integer    | Duration before being thirsty     |
|                       |            | again once having drunk.          |
+-----------------------+------------+-----------------------------------+
| ``next_thirsty_tick`` | Integer    | Next moment at which this customer|
|                       |            | will be thirsty again.            |
+-----------------------+------------+-----------------------------------+
| ``current_money``     | Integer    | How much money this customer still|
|                       |            | has in his pocket.                |
+-----------------------+------------+-----------------------------------+


In terms of behaviour, a customer will buy soda and wait until being thirsty again, then he will buy soda, etc. This cycle will stop only once:

	- the customer has no money left
	- and/or the machine has no can left

Initially, the customer will not be thirsty at all.

The customer may have to handle requests of other customers who want to borrow a specified amount of money from him. He can decide to accept only if having enough money to do so, based on any internal logic he may rely on, and after a duration of his choice. The other customer is expected to wait indefinitely for his answer (no time-out, a customer is supposed to always respond).

At any time a customer may be involved into at most one interaction.



Interaction of the Soda Vending Machine and of the Thirsty Customer Models
--------------------------------------------------------------------------

A high-level description of the behaviour of each model can be specified thanks to a dedicated diagram, inspired from `Finite State Machines <http://en.wikipedia.org/wiki/Finite_state_machine>`_ (FSM).

The following graphical conventions are used here:

:raw-html:`<img src="FSM-legend.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{FSM-legend.png}`

Not specifying an event condition on a state transition means here that the state change is time-based, i.e. it will occur automatically once a specific duration (in simulation time) will be elapsed.

The two models are interacting, thus the two FSM will interact as well, based on message exchanges:

:raw-html:`<img src="soda-ordering-interacting-FSM.png"></img>`
:raw-latex:`\includegraphics[scale=0.4]{soda-ordering-interacting-FSM.png}`


(the inter-customer exchanges are not shown here)


Implementations
---------------

The corresponding files are available in the ``sim-diasca/src/models/examples/src`` directory:

 - ``class_SodaVendingMachine.erl``: the vending machine model

 - ``class_DeterministicThirstyCustomer.erl``: the deterministic customer

 - ``soda_deterministic_integration_test.erl``: a corresponding simulation test case



Implementation of the Integration Test
......................................

As we already determined what parameters will be given to created instances, we can already set up what can be a proper integration test. It could be stored in the ``soda_deterministic_integration_test.erl`` file.

Once run, it creates two machines and three customers, and then starts the simulator until no customer can buy anything anymore and/or all machines exhausted their can cost.

The commented source code is pretty self-explanatory.


Implementation of the Soda Vending Machine
..........................................

We chose to implement the soda vending machine as a basic simulation actor (``class_Actor``).

As the model of the machine does not exhibit any spontaneous behaviour, its ``act`` method will be mostly empty [#]_::

  % The core of the soda vending machine behaviour.
  % (oneway)
  act(State) ->
	  % Here a machine as no spontaneous behaviour, so it does not do
	  % anything special.
	  ?wooper_return_state_only( class_Actor:manage_end_of_tick(State) ).


.. [#] It could have even been left undefined, so that the default do-nothing implementation could be naturally inherited.


The heart of the machine behaviour is in its ``orderSoda`` method instead. It will be triggered by a customer, and in return will trigger one of the following methods on the customer's side:

	- ``getCan``, if the transaction succeeded
	- ``onNoCanAvailable``, if the transaction failed due to a lack of can in the machine
	- ``onNotEnoughMoney``, if the transaction failed due to insufficient funds given to the machine

depending on:

	- the remaining stock available in that vending machine
	- the remaining budget of the customer

Of course the state of both parties will be updated accordingly: after a successful transaction, there will be one fewer can in the machine, and a somewhat poorer, but less thirsty, customer.


That ``orderSoda`` method could have been simply a request method (hence returning directly a result to the customer) instead of an actor oneway (i.e. based on synchronised actor messages being exchanged asynchronously, with no direct answer returned), but then the mechanisms maintaining the simulation properties would not be involved and, notably, reproducibility would be lost.

Therefore a request directly resulting in an answer must be replaced by one actor oneway call that will trigger back another actor oneway call later, at the next simulation tick.

Finally the ``orderSoda`` method remained quite simple::

   % Called by a customer wanting to purchase a can.
   % (actor oneway)
   orderSoda(State,CustomerPid) ->


We can see the machine is given the customer budget, so that it can tell whether all conditions are met so that a transaction succeeds.

There is a lot of room for improvement in that simple test case:

	- money could be collected coin-per-coin until the cost of a can is reached
	- the ordering of a soda could last for longer than one simulation tick, and the duration could depend on the outcome of the transaction
	- here we considered that one simulation tick corresponded to 1 virtual minute, we should instead make the duration independent from the scheduler frequency (ex: by using ``class_Actor:convertSecondsToTicks/2``)
	- more than one can could be bought at once (i.e. in one interaction)
	- multiple kinds of sodas could be offered
	- the machine could request spontaneously the operating staff to renew its stock
	- an employee of the soda vendor could come regularly and refill the machine
	- the machine could fail and be repaired
	- the model of the machine could be run at a frequency lower than the fundamental one of the simulation or, still better, could be purely event-driven (i.e. not scheduled by the time manager on a regular basis), since it has here no spontaneous behaviour
	- etc.



Implementation of the Thirsty Customer
......................................

We chose to implement this *deterministic* thirsty customer as a basic simulation actor (``class_Actor``).

There is a lot of room for improvement:

	- a customer might use multiple vending machines and optimise his purchasing depending on cost and availability

	- money could be inserted coin-per-coin by the customer, until the cost of a can is reached

	- a thirsty customer could stop trying to buy cans from a machine he knows having none

	- etc.



Stochastic Models
=================

We wrote a second example in the context of soda vending machines, in which stochastic actors are simulated: this time, some customers are driven by a user-defined random law, which determines when they will be thristy again.

One customer will thus be thirsty again after a duration between 1 and 10 minutes (with all durations in-between having equal probability) after having drunk, whereas the other is thirsty according to a Gaussian (normal) law, on average 3 minutes after having drunk with a variance of 1.

The third customer will be still deterministic, to show that these models can coexist in the same simulation.


The corresponding files are available in the ``sim-diasca/src/models/examples/src`` directory:

 - ``class_SodaVendingMachine.erl``: the vending machine model (unchanged)

 - ``class_StochasticThirstyCustomer.erl``: the stochastic customer

 - ``class_DeterministicThirstyCustomer.erl``: the deterministic customer (unchanged)

 - ``soda_deterministic_integration_test.erl``: a corresponding simulation test case
