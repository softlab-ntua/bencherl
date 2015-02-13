:raw-latex:`\pagebreak`

-------------------------------------------
Sim-Diasca Management of Simulation Outputs
-------------------------------------------


Principles
==========

A simulator allows to run virtual experiments, involving input parameters and models, and producing corresponding outputs, a subset of it being tagged as the actual results awaited by the user. Some post-processing can then be later applied to these results, which are usually rather raw by-products.

The overall goal is to obtain, from known facts and thanks the simulations, new knowledge that was not necessarily anticipated.

In the same way as one may rely on a (hopefully validated_) oracle or a gyroscope, the outcome of a simulation cannot be really precisely foreseen (otherwise it would be plain useless):

:raw-html:`<img src="xkcd-gyroscopes.png"></img>`
:raw-latex:`\includegraphics[scale=0.45]{xkcd-gyroscopes.png}`




Managing The Outputs
====================


General Mode of Operation
-------------------------

Sim-Diasca offers a fully concurrent support for simulation results (from their generation to their retrieval), so that they are automatically returned back to the user in its launching directory, knowing that on a distributed context the output data is by design scattered across the various networked computing nodes.

For that, if and if only the simulation finishes successfully, a directory specific to that simulation run (based on simulation name, time and date, user and a rather unique identifier [#]_) is created in the current directory (i.e. the directory from which ``make My_Test_run`` was executed) of the user node, bearing a name like::

 Sim-Diasca_My_Test-on-2012-12-10-at-10h-05m-31s-by-boudevil-1f793a6ba507

Results of interest are automatically transferred there (otherwise one would have to log in on each and every computing node [#]_ to select and retrieve these results by hand).


.. [#] An effort is made to generate names for result directories that can hardly collide, as for example in the context of parametric studies a launcher script might launch, from the same location and user, more than one simulation case per second.


.. [#] All computing nodes uses a temporary directory for their work, which includes extracting the deployment archive to access to the simulation input data and code, and storing locally the results being produced. The name of this directory (under ``/tmp/``) is forged to be reasonably unique (ex: ``sim-diasca-My_Case-boudevil-2012-12-7-at-13h-56m-03s-1f793a6ba507``), to avoid clashes between simulations that would run concurrently on shared computing hosts.



Results are handled by:

 - the overall *Result Manager* (a ``class_ResultManager`` singleton, automatically created at deployment time on the user node), which keeps track of results and drive them

 - *Result producers* (all ``class_ResultProducer`` instances, like probes, either basic or datalogger-based), which provide support to declare, aggregate and send their results


Following mode of operation allows to handle results:

 #. the simulation user specifies initially, in the simulation case, what are the results he is interested in, thanks to a result specification

 #. the result manager checks these input, and precomputes what will be needed to discriminate between wanted and unwanted simulation outputs

 #. while actors and producers are created (either initially or at simulation-time), result producers declare themselves automatically to the result manager, which is able to tell whether their output is to be promoted to simulation result

 #. when (if) the simulation ends successfully, the result manager automatically requests the relevant results from all relevant producers, and copy them in the result directory (more precisely, it generates them only if appropriate, like in the case of plots, and send them in a compressed from over the network, to the user node)



Result Specification
--------------------

Which outputs are to be promoted to results is basically to be specified among the simulation settings, and based on the names of the result producers to target (as it is the only way the user can refer to them *a priori*, as of course PID cannot be known then).

The outputs that this simulation regards as results that are actually
needed are the ones that correspond to the result specification for that
simulation.

The simulation case may specify (possibly with additional result generation settings) that:

 - all or no output is to be selected
 - only the outputs corresponding to plain probes, or to virtual ones (i.e. based on the data-logger) are wanted
 - only outputs whose name matches at least one targeted pattern, and none of the blacklisted patterns, are to be selected (most general case)


Patterns are to be expressed according to the “Perl Compatible Regular
Expressions” conventions, or PCRE for short.

For more information, see following `cheat sheet <http://www.bitcetera.com/page_attachments/0000/0030/regex_in_a_nutshell.pdf>`_ and the `re module <http://erlang.org/doc/man/re.html>`_.

:raw-html:`<img src="xkcd-perl_problems.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-perl_problems.png}`


The detailed supported syntax is specified in the ``sim-diasca/src/core/src/scheduling/class_TimeManager.hrl`` header file (see the ``result_specification`` field of the ``simulation_settings`` record).


Relying on these simulation settings allows to define results *statically*, which is fine for most uses. However, under some circumstances, it may be convenient to set or modify the result specification *dynamically* (ex: if it is difficult to anticipate on the name of a probe).

Thus result specification can be also modified at simulation-time, thanks to method calls (see the ``{add,remove,set}{Targeted,Blacklisted}Pattern*/2`` methods of ``class_ResultManager``).



Early Disabling of Outputs
--------------------------

All results could be generated in all cases, and only be retrieved if requested.

However a better approach could be to collect data samples and process them (ex: in graphical plots) only if needed.

A still better approach is needed: as the result manager is able to tell directly whether a result is wanted, it will be able to disable unwanted outputs from the start, i.e. reject any attempt of creating a result producer (ex: a probe) whose results are not wanted by the user.

As a result, a probe should be created thanks to the ``class_Probe:create/5`` static method, which will return either the PID of this newly created probe (if the name of that probe is acknowledged as a wanted result by the result manager), or the ``non_wanted_probe`` atom.



Result Generation
=================

Often, many models are able to define various probes, and the corresponding number of instances is huge.

This results on a large number of result producers, even after having selecting only a subset of them (thanks to the result specification language).

The consequence is that the parallel, distributed result generation cannot be triggered as a whole, lest the most loaded computing nodes crash.

The result manager therefore implements a flow control mechanism, ensuring that all possible computing nodes work at full speed, while not being too much overloaded. Basically, at any time, up to twice as many generations are requested as they are cores on a given host. Any generation completion yields the requesting of another pending one (if any).



Post-Processing The Results
===========================

Some approaches and tools can be used to transform results into knowledge. This involves generally synthesising the vast amount of data into a few relevant statistics or indicators.

The post-processing to be done depends significantly on the specific problem being studied. Currently, except probe reports, Sim-Diasca outputs mainly time series, letting the user feed these raw data to the suitable tools, on a problem-specific way.



Interpreting The Outcome
========================

Once the right questions have been properly formalised, this is probably, with the validation_ part, the trickiest part of a simulation work: what are the lessons learned, and how much can be trust them?

Providing detailed guidelines would be beyond the scope of this document, here are nevertheless a few hints.


Identifying Reasons For Observed Phenomena
------------------------------------------

Finding actual causes is seldom straightforward:

:raw-html:`<img src="xkcd-correlation.png"></img>`
:raw-latex:`\includegraphics[scale=0.8]{xkcd-correlation.png}`



Having Reasonable Expectations
------------------------------

A simulation is not the silver bullet that will ask the right questions on the user's behalf and answer them with infinite accuracy:

:raw-html:`<img src="xkcd-science_montage.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-science_montage.png}`

Simulation being a rather expensive and time-consuming mode of evaluation, it should be used on carefully selected cases that cannot be solved satisfactorily thanks to other methods, like comparison with actual systems, expert assessments, coarse spreadsheet-based studies, etc.

Even in that case, a few well-selected metrics must be defined, that must be both helpful to the user and solvable by the simulation.



Extrapolating Results, Really?
------------------------------

Unless it has been proven separately, one cannot arbitrarily reduce the problem size and expect that a small-scale experiment will still provide reliable insights about a real-sized system: `reductionism <http://en.wikipedia.org/wiki/Reductionism#Reductionism_and_science>`_ cannot be applied blindly.

This is why the scalability of a simulation engine is a key property: whenever smaller-scale experiments cannot be safely attempted (the general case), it offers a better chance of capturing the reality.

Indeed extrapolating becomes too often a wild guess:

:raw-html:`<img src="xkcd-extrapolating.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-extrapolating.png}`


In most cases, approaches based on extrapolations are hardly sustainable:

:raw-html:`<img src="xkcd-sustainable.png"></img>`
:raw-latex:`\includegraphics[scale=0.45]{xkcd-sustainable.png}`



Sharing The Findings With The Intended Audience
-----------------------------------------------

The lessons learned thanks to the simulation must be synthesised appropriately, with proper wording for the targeted public, so that the conclusions are sufficiently emphasized to be well-understood:

:raw-html:`<img src="xkcd-simple.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-simple.png}`


Concerns must be correctly shared among the people involved, with appropriate common metrics and goals:

:raw-html:`<img src="xkcd-car_problems.png"></img>`
:raw-latex:`\includegraphics[scale=0.55]{xkcd-car_problems.png}`



Making Good Use Of The New Knowledge
------------------------------------

It is certainly out of the scope of this document, but simulations may generate new knowledge, which must be carefully leveraged, lest it worsens the situation:

:raw-html:`<img src="xkcd-conditional_risk.png"></img>`
:raw-latex:`\includegraphics[scale=0.8]{xkcd-conditional_risk.png}`
