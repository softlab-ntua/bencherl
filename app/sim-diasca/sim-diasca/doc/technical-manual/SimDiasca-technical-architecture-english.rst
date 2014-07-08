:raw-latex:`\pagebreak`

---------------------------------
Sim-Diasca Technical Architecture
---------------------------------

General View
============



:raw-html:`<img src="technical-architecture-english.png"></img>`
:raw-latex:`\includegraphics[scale=0.4]{technical-architecture-english.png}`



:raw-latex:`\pagebreak`


Supported Platforms
===================

Sim-Diasca is basically able to run on most UNIX platforms, although we mostly focus on the GNU/Linux one.

Builds on MacOS have been reported, and the Windows systems could be targeted, but these platforms are deemed of little interest in our context.



More generally, the ability to run on clusters (``[S7]``)  improves a lot the performances, not only because of the high-level of RAM and CPU they provide, but also, and maybe to a larger extent, because of the high-performance network links they makes use of: the shorter a tick lasts, the more low-latency networks, like Infiniband, boost the overall simulation performance.

The PBS-based clusters are now fully supported (see ``sim-diasca/conf/clusters/sim-diasca-launcher.sh``), we use routinely a cluster based on ``Torque+MAUI``. Other cluster scripts have been developed to respectively deploy, launch and collect results for *sets* of experiments (ex: automating the launching of 72 simulations, to perform a kind of parametric studies).



Tools For the Sim-Diasca Simulation Engine Itself
=============================================


Erlang
------

Basically, `Erlang <http://www.erlang.org>`_ provides a full environment particularly suitable for multi-agent applications.

At this lower level of the architecture, we are dealing with *Erlang processes*, which are lightweight objects:

 - having each their own execution thread (their mode of operation is inherently concurrent)
 - communicating between them only thanks to messages (pure asynchronous message passing, no memory is shared)

Erlang processes are not mapped to any scheduling object provided by the operating system or by the general execution environment. Thus Erlang processes are not system processes nor threads.

The Erlang virtual machine schedules itself all the Erlang processes it is hosting [#]_. This is why the number of concurrent processes within the Erlang virtual machine can strongly outperform the number that could be achieved with any system-level thread of execution, as the overhead that Erlang processes induce is considerably smaller.

.. [#] This is an example of the so-called *green threads*.

Erlang processes execute *functions* that are gathered in *modules*. Being Erlang code, they are implemented in a declarative, functional way, with single-assignment, absence of side-effects, pattern-matching and recursive behaviour:

:raw-html:`<img src="xkcd-dependencies.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-dependencies.png}`


Although a bit disconcerting at first glance, these kinds of languages are very suitable to develop complex algorithms and, more generally, scalable and reliable software.

Notably they offer higher-level constructs that allow Erlang programs written for one node to be distributed on a set of machines seamlessly, i.e. with little or no changes in the code, since Erlang processes will in both cases only exchange messages, regardless of their actual location (either on the same node or in networked nodes).

Erlang is an open source software, and has been used for more than 20 years. Its future is bright: now that physical limits are almost reached, microprocessors are less and less able to increase their operating frequencies, therefore the only solution that remains is to multiply the number of cores and try to use them effectively.

However traditional sequential languages cannot cope gracefully with such a parallel context, as they cannot deal with concurrent accesses to shared memory without error-prone and resource-consuming solutions, like mutex and semaphores. Therefore concurrency-based languages like Erlang should be increasingly needed in the years to come.



WOOPER
------

Erlang offers very suitable base services for our requirements, but when developing simulations one extra property would help a lot: the ability to model and implement simulated elements according to the Object-Oriented Paradigm.

Indeed, the modelling of numerous and complex behaviours is easier if being able to use the classical concepts of OOP like components, classes, instances, remote method invocations, inheritance, polymorphism, state management, etc.

Implementation is itself easier if the language supports some way of relying on a direct mapping between the OOP modelling concepts and the nuts and bolts offered by that language.

As classical Erlang is process-based and declarative, the OOP constructs have to added to the language. This is the task of `WOOPER <http://ceylan.sourceforge.net/main/documentation/wooper/>`_, which stands for *Wrapper for OOP in Erlang*.

WOOPER is a very lightweight layer that adds some code and conventions to Erlang so that a full-blown OOP approach can be applied directly to the language, at the expense of very little additional developing efforts and only a small run-time overhead.

Therefore, from that level on, we will not speak in terms of Erlang processes any more, we will mostly be dealing with instances of WOOPER classes.

WOOPER is an open source software (LGPL license).


Sim-Diasca
----------

Such WOOPER instances are however not simulation actors yet: the support for the already mentioned mechanisms required in the context of a distributed simulation must be added, otherwise causality, reproducibility, etc. would not be ensured.

This is the task of the ``core`` component of the Sim-Diasca simulation engine: it provides the required technical components (like the ``TimeManager`` and the ``RandomManager``) and the counterpart behaviours that all simulation actors should develop to interact properly with these technical components.

More precisely, Sim-Diasca Core provides the ``Actor`` class, from which all Sim-Diasca models should inherit (directly or not). Then they will automatically embed all the necessary logic to interact with the ``TimeManager``, which includes managing ``top`` messages, tracking transparently acknowledgements of sent actor messages, dealing with errors and appropriate ends of ticks, etc.

Actors making use of random variables have also to interact correctly with the  ``RandomManager``. This is done similarly, just by inheriting from the ``StochasticActor`` class, which itself is a child class of the ``Actor`` class. Then all the mechanisms to find and use the ``RandomManager`` will be readily available, like for example the algorithm to maintain automatically a buffer of cached random values.

Finally, thanks to these inheritances, the development of models will mostly consist on specifying the business-specific state changes and message exchanges supported by each type of simulated element. Most technical issues are hidden to the model developer, who will only have to define:

 - how an actor will be initialised (i.e. the constructor of its class)
 - how an actor will be deleted (i.e. the destructor of its class)
 - how an actor will behave spontaneously at each tick (i.e. its ``act`` method)
 - any other behaviours that could be triggered by notifications received from other actors (i.e. the methods other simulation actors might call, thanks to actor messages)

These are totally model-specific, no simulation mechanism can provide them, only the model developer can know which code is relevant here.

``Sim-Diasca Core`` provides as well useful technical components, like a full distribute trace system to be used by simulations.



Complementary Tools
===================

A few third-party tools are used in the context of Sim-Diasca. They are not direct parts of the simulation engine, but they are very useful to make a better use of the framework.

As they are already wrapped by the appropriate Sim-Diasca code, they will be automatically triggered and used by the simulator, with no further action from the user.


LogMX
-----

`LogMX <http://www.logmx.com/>`_ is a simple yet quite powerful tool to view logs. In the context of Sim-Diasca it is the main part of the supervisor of simulation traces.

As stated earlier, a simulation-specific format for traces is needed, and of course LogMX cannot know it *a priori*. Therefore a small LogMX-compliant trace parser, written in Java, has been developed, which integrates to LogMX. This is the only bit of Java involved in Sim-Diasca.

LogMX is a rather inexpensive tool (at most $29 per user), and Sim-Diasca can make use of its evaluation version as well.


gnuplot
-------

`gnuplot <http://www.gnuplot.info/>`_ is a very well-known portable data and function plotting utility.

It is notably used by Sim-Diasca probes when they are requested to output a graphical view of their state: they automatically generate the appropriate command and data files, then call gnuplot to have it render the corresponding curves in a graphic file that might be displayed if wanted.

gnuplot is freely distributed.

.. Warning:: Ensure that the ``gnuplot`` version installed on your system is not too obsolete. Version 4.2 and higher is recommended, otherwise the generation of some graph renderings might fail.



Graphviz Dot
------------

`Graphviz <http://www.graphviz.org/>`_ is another quite widespread tool, which is a graph visualisation software.

Sim-Diasca uses it to generate graphical views of meshes: a ``Mesh`` (directed graph) is able to output a suitable description of this vertices and edges so that the ``dot`` program can generate from it a graphic file that might be displayed if wanted.

Various models make use of such meshes, like the ``LowVoltageMesh``.

Graphviz (including dot) is an open source software.



Image Viewer
------------

When Sim-Diasca needs to display a graphic file, it can drive various tools to do so, including the ``eog`` viewer, which is open source.


Mplayer/Mencoder
----------------

`Mplayer <http://www.mplayerhq.hu>`_ is an open source software package that allows, among other things, to generate movies from a set of image files (with ``mencoder``), and to display them (with ``mplayer``).

Sim-Diasca uses them to aggregate a set of time stamped frames (each frame corresponding to one simulation tick) into a movie, so that the changes over time of graphical simulation results can be better monitored by the user.

For example, if, for a mesh, the generation of a time-based description of its state has been requested, a corresponding movie can be generated.




Other Tools
===========

They are not used at execution-time (i.e. during a simulation), but they are nevertheless involved in our daily usage of Sim-Diasca.


GNU make
--------

The `GNU make <http://www.gnu.org/software/make/manual/make.html>`_ utility, which determines automatically which pieces of a large program need to be recompiled, and issues the commands to recompile them, is intensively used by Sim-Diasca, to build and run the simulator itself but also to post-process some of its results.

GNU make is open source.



Version Control: GIT
--------------------

`GIT <http://git-scm.com/>`_, is a free and open source distributed version control system. It allows to keep track of the changes of the Sim-Diasca source code, and to share it among developers.


Docutils
--------

`Docutils <http://docutils.sourceforge.net/>`_ is a set of open source documentation utilities. It operates on text files respecting the *reStructuredText* mark-up syntax, and is able to generate from it various formats, including LateX (hence PDF) and HTML.

This document has been generated thanks to Docutils.
