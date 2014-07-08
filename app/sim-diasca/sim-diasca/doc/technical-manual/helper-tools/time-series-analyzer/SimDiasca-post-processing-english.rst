Post-Processing Services
========================

As hinted by their name, these services are not integrated to the engine, but are to be applied, if needed, to the results it produces.


Time-Series Analyzer
--------------------

Time-series are typically produced by simulation probes (either basic or virtual), and are stored in ``*.dat`` files, which gathers the values of a series of curves at different ticks.

Such files can be inputs to the Sim-Diasca analyzer tool for time series, which can then apply them various algorithms, some related to signal processings.

For a time-series file, identified processings of interest are:

 - select a subset of curves in a time-series file

 - remove empty ones and/or constant ones

 - generate a basic ``gnuplot`` command file corresponding to a ``.dat`` file

For a curve,  identified processings of interest are:

 - find the extrema (minimum and maximum values), and when they occur (list of matching ticks)

 - compute the average value of the curve (which is not necessarily the average of the values, due to potentially non-consecutive ticks)

 - apply a convolution filter on the curve samples

 - compute the curve for the moving average (a specific case of convolution), which is a way of obtaining a smoother curve, making its interpretation easier by highlighting trends

 - perform a linear interpolation between samples

 - perform decimation, to reduce the number of samples

 - change scale (ex: switch to logarithmic, reshape to fit into a ``0..1`` interval, etc.)


:raw-html:`<img src="xkcd-log_scale.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-log_scale.png}`
