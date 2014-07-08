:raw-latex:`\pagebreak`


.. _validation:
.. _validated:


-----------------------------------
Validating The Resulting Simulators
-----------------------------------

Adding to Sim-Diasca (the simulation engine) a set of models and at least one simulation case results into building a new simulator.

This simulator may or may not be accurate, and this must be established first.

Indeed, drawing bad conclusions from false assumptions or improper reasoning is surprisingly easy:

:raw-html:`<img src="xkcd-dimensional_analysis.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-dimensional_analysis.png}`

Being able to trust this new simulator is of course of the utmost importance.

Therefore, before using it to learn new facts, one must validate it adequately.


Validation can be done thanks to multiple non-exclusive approaches, mostly based on the produced results:

  - having field experts review virtual experiments about known situations
  - computing coarse orders of magnitude with other approaches (ex: spreadsheet-based estimations not involving time aspects)
  - checking the results against an actual (in real-life) test bed, provided that early prototypes are available
  - comparing a representative set of outputs to the one produced by a reference simulator (ex: validating the telecom aspects of a business simulation against an industrial-grade telecom-specific simulator)
