---------------
Developer Hints
---------------


Choosing The Right Datastructures
=================================

Writing models involves a lot of algorithmic design decisions, and many of them deal with data-structures.

:raw-html:`<img src="xkcd-set_theory.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-set_theory.png}`


There is a large choice of both data-structures as such (lists, trees, associative tables, heap, etc.) and of implementations (``gb_tree``, hashtables, etc.), offering various trade-offs.

In some occasions, we felt the need to develop our own versions of some of them (see ``common/src/data-management`` for the most common ones), even if in some cases built-in solutions could provide better trade-offs (thinking to ETS tables- albeit offering a different sharing semantics - and to the process dictionary - which is not the purest and most flexible feature we wanted to rely on).

We nevertheless use most of the time the built-in data-structures, like ``gb_sets`` or ``queues``. When multiple implementations providing the same API are available (ex: for ordered lists), we usually define a (sometimes module-specific) ``list_impl`` symbol, allowing to switch easily between similar data-structures.

For example::

	% Defines list_impl:
	-include("data_types.hrl").

	f( A ) ->
	  true = ?list_impl:is_empty( A ), [...]


:raw-html:`<img src="xkcd-tree.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-tree.png}`


Similarly, for **algorithms** operating on these data-structures we tend not to reinvent the wheel (ex: ``class_Mesh`` uses the ``digraph`` module), unless we need specific versions of them (ex: operating on an implicit graph, with user-specified anonymous functions, see ``common/src/utils/graph_utils.erl``).

:raw-html:`<img src="xkcd-.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-ineffective_sorts.png}`



Running Bullet-Proof Experiments
================================

Making use of large-scale HPC infrastructures is not straightforward: they often behave like black boxes, and because of the number and specificities of the hardware and software elements that are involved, they tend to exhibit unanticipated behaviours.

Here is a list of recommended steps to go through, in order to have a better chance of making good use of these resources:

 #. deactivate the sending of simulation traces, that would otherwise overwhelm the trace aggregator: comment-out, in ``traces/src/class_TraceEmitter.hrl``::

	 %-define(TracingActivated,)

 #. activate any console outputs you are interested in (uncomment relevant ``io:format calls``):

	#. in ``sim-diasca/src/core/src/scheduling/class_LoadBalancer.erl``, in the ``create_actor/4`` function, if wanting to follow actor creation

	#. in ``sim-diasca/src/core/src/scheduling/class_DeploymentManager.erl``, in the ``launch_node/4`` function, if wanting to measure how long it took to accept or reject each candidate host (it may last for more than one full minute in some cases)

 #. increase the time-outs:

	#. in ``wooper/src/wooper.hrl``, one may uncomment the extended ``synchronous_time_out`` constant recommended for simulation

	#. in extreme conditions, in ``sim-diasca/src/core/src/scheduling/class_TimeManager.erl``, in the ``watchdog_main_loop/3`` function, ``MaxMinutes`` could be increased

	#. still in extreme conditions, in ``sim-diasca/src/core/src/scheduling/class_DeploymentManager.erl``, in the ``launch_node/4`` function, at the level of the ``net_utils:check_node_availability`` call, ``AttemptCount`` could be increased (trade deployment speed for reliability)

 #. copy the source of the simulator and update the configuration files according to the case to be run

 #. compile everything from the root, from scratch (``make clean all``)

 #. for all the classes for which traces are wanted (if any), re-enable their sending:

   - re-activate traces, reverting the content of ``traces/src/class_TraceEmitter.hrl``
   - update the time-stamps of all target classes, ex::

	 touch sim-diasca/src/core/src/scheduling/class_DeploymentManager.erl

 #. re-compile from the root (``make all``) if traces were enabled for at least one class

 #. possibly: hide ``~/.ssh/known_hosts`` to avoid nodes being rejected because of a change in the RSA fingerprint of their key

 #. launch in debug mode from the front-end, ex::

	 sim-diasca/conf/clusters/sim-diasca-launcher.sh --debug
	  --node-count 32 --cores-per-node 8 --queue parall_256
	  --max-duration 64 foobar-simulator/src/uc23_integration_test.erl
