:raw-latex:`\pagebreak`


--------------------------
Sim-Diasca Functional Coverage
--------------------------

Based on the property list discussed in `List of Spotted Potential Properties For the Simulator`_, we tried to evaluate what are the features currently offered by Sim-Diasca in the following table.


+------------------------+-------------------+----------------------------------+
| Property Identifier    | Estimated Quality | Comments                         |
|                        | of Sim-Diasca     |                                  |
|                        | support           |                                  |
|                        | for that property |                                  |
+========================+===================+==================================+
| P1: causality          | Fully supported   | No effect should occur before    |
| preservation           |                   | its cause since it will be       |
|                        |                   | managed at least one simulation  |
|                        |                   | tick later.                      |
+------------------------+-------------------+----------------------------------+
| P2: reproducibility    | Fully supported   | A reproducible total order on    |
|                        |                   | simulation events is enforced,   |
|                        |                   | (provided of course that models  |
|                        |                   | respect the Sim-Diasca           |
|                        |                   | conventions).                    |
+------------------------+-------------------+----------------------------------+
| P3: discrete events    | Fully supported   | Events happen relatively to a    |
|                        |                   | given simulation tick.           |
+------------------------+-------------------+----------------------------------+
| P4: steady state       | Fully supported   | Special case of P5.              |
+------------------------+-------------------+----------------------------------+
| P5: dynamic state      | Fully supported   | Models are free to develop       |
|                        |                   | *any* behaviour over time.       |
+------------------------+-------------------+----------------------------------+
| P6: stochastic actors  | Fully supported   | There are two generic mechanisms |
|                        |                   | to support all kinds of          |
|                        |                   | probability distributions. Most  |
|                        |                   | basic laws (uniform, Gaussian    |
|                        |                   | and exponential) are built-in.   |
+------------------------+-------------------+----------------------------------+
| P7: batch mode         | Fully supported   | This is the default mode of      |
|                        |                   | operation.                       |
+------------------------+-------------------+----------------------------------+
| P8: interactive mode   | Fully supported   | Easy to provide effectively with |
|                        |                   | time-stepped simulations. Of     |
|                        |                   | course does not guarantee that   |
|                        |                   | simulations will be fast enough  |
|                        |                   | to keep up with the user time    |
|                        |                   | (depends mostly on the available |
|                        |                   | computing resources).            |
+------------------------+-------------------+----------------------------------+
| P9: standardised traces| Fully supported   | A fairly advanced system allows  |
|                        |                   | to emit distributed traces,      |
|                        |                   | aggregate them and monitor them, |
|                        |                   | but they are currently managed   |
|                        |                   | for the user: they are not       |
|                        |                   | especially designed for          |
|                        |                   | third-party tools.               |
+------------------------+-------------------+----------------------------------+
| P10: result management | Supported         | Currently most results come from |
|                        |                   | the probes and the data-logger,  |
|                        |                   | both of which are built-in.      |
+------------------------+-------------------+----------------------------------+
| P11: parallel operation| Fully supported   | Provided by the time-stepped     |
| (algorithmic)          |                   | approach (all actors can run     |
|                        |                   | concurrently at each time step). |
+------------------------+-------------------+----------------------------------+
| P12: parallel operation| Fully supported   | Provided by the Erlang runtime   |
| (technical)            |                   | (one Erlang virtual machine per  |
|                        |                   | processor and/or core).          |
+------------------------+-------------------+----------------------------------+
| P13: distributed       | Fully Supported   | The engine can run on any set of |
|                        |                   | networked (UNIX-based) computing |
|                        |                   | nodes, including High Performance|
|                        |                   | Computing clusters (full suite of|
|                        |                   | management scripts provided for  |
|                        |                   | PBS-based clusters).             |
+------------------------+-------------------+----------------------------------+
| P14: use of HPC        | Fully Supported   | The engine is able to run on     |
| resources              |                   | PBS-based HPC clusters, and      |
|                        |                   | some manycore architectures      |
|                        |                   | (Tilera cards) will be           |
|                        |                   | supported soon.                  |
+------------------------+-------------------+----------------------------------+
| P15: extensibility     | Fully supported   | Models can be added very easily, |
|                        |                   | with no additional compilation.  |
|                        |                   | Some practise with the Erlang    |
|                        |                   | language is needed.              |
|                        |                   | As few constraints as possible   |
|                        |                   | weight on models.                |
+------------------------+-------------------+----------------------------------+
| P16: high-level        | Moderate support  | No model-specific language is    |
| modelling language     |                   | used, but the high-level         |
|                        |                   | constructs of Erlang, the        |
|                        |                   | OOP services of WOOPER and the   |
|                        |                   | simulation services of Sim-Diasca|
|                        |                   | are available and can be easily  |
|                        |                   | reused. Nothing though that can  |
|                        |                   | be compared with formal proof or |
|                        |                   | model checking.                  |
+------------------------+-------------------+----------------------------------+
| P17: integration with  | Not supported yet | Although this is theoretically   |
| real devices           |                   | feasible, this topic has not     |
|                        |                   | been explored yet.               |
+------------------------+-------------------+----------------------------------+
| P18: model composition | Supported         | A model can create, remove,      |
|                        |                   | configure, update other models.  |
|                        |                   | Composition must be done with    |
|                        |                   | care though, to deal with any    |
|                        |                   | latency induced by layers of     |
|                        |                   | models.                          |
+------------------------+-------------------+----------------------------------+
| P19: interface to      | Fully Supported   | Erlang provides several means    |
| third-party tools      |                   | of doing so, which is especially |
|                        |                   | useful for post-processing.      |
+------------------------+-------------------+----------------------------------+
| P20: open-source       | Fully Supported   | Erlang and WOOPER are            |
|                        |                   | open-source, and as of September |
|                        |                   | 2010 Sim-Diasca has been         |
|                        |                   | released.by EDF R&D in LGPL.     |
+------------------------+-------------------+----------------------------------+
| P21: result management | Fully Supported   | Results are automatically        |
|                        |                   | selected, produced, retrieved    |
|                        |                   | despite their distribution.      |
|                        |                   | The result management is both    |
|                        |                   | very flexible (smart             |
|                        |                   | specification language) and      |
|                        |                   | efficient (only relevant results |
|                        |                   | are produced).                   |
+------------------------+-------------------+----------------------------------+
| P22: reliability       | Supported         | Simulation will properly crash   |
|                        |                   | if any constraint is violated.   |
|                        |                   | A simulation stall/deadlock      |
|                        |                   | detection and diagnosis system   |
|                        |                   | will be triggered if ever issues |
|                        |                   | arise (ex: faulty model).        |
+------------------------+-------------------+----------------------------------+


The Sim-Diasca functional coverage increased over time, first to support a better distributed mode of operation, then to provide more complete support for the management of results.

Although we cannot guarantee that they will be fulfilled, we are always interested into requests for enhancement, provided they are reasonable:

:raw-html:`<img src="xkcd-spinal_tap_amps.png"></img>`
:raw-latex:`\includegraphics[scale=0.55]{xkcd-spinal_tap_amps.png}`


The planned future changes are listed in the enhancements_ section.
