
Spatialised Support
===================


Overview
--------

Currently only a support for 2D environments is provided - of course the user might define any number of other environments if wanted.

The spatial environment is embodied by an instance (a singleton) of ``class_TwoDimensionalEnvironment``.

The support for low-level spatial computations is provided by the ``linear_2D`` module (in ``common/src/maths``).

Distances are expressed in meters, speeds in meters per second.

The simulated world has an origin (``{0.0,0.0}``) and spreads in both dimensions. Cartesian coordinates are used.

By default the world is a torus, i.e. the coordinates wrap around. They range from the origin to ``{XMax,YMax}``.




Spatialised Elements
--------------------

A simulation element that is  *spatialized* has a position in the simulation world, and should be (directly or not) an instance of ``class_SpatializedEntity`` or, more precisely here, a ``class_TwoDimensionalSpatializedActor``.

The constructors of each of these classes take, as first actual parameter, the PID of the environment. An upper bound of the speed of each instance may be specified (it allows to optimise the mode of operation of the environment).

This ``class_TwoDimensionalSpatializedActor`` class defines an attribute ``position`` whose type is ``linear_2D:point()`` and a ``-spec getPosition(wooper_state(),pid())`` actor oneway which triggers back on the caller, at the next diasca, the ``-spec notifyPosition(wooper_state(),linear_2D:point(),pid())`` actor oneway.




Interaction with the Environment
--------------------------------

In the general case, no instance is able to know the whole simulation world. An instance is only able to perceive a subset of it, through its perception.

Such a neighborhood can only be obtained thanks to a specific actor, the environment.

An instance can indeed call the actor oneway defined, here in ``class_TwoDimensionalEnvironment``, as ``-spec getEntitiesWithin(point(),radius(),pid())``: it requests that the entities within a disc whose center and radius are specified are determined by the environment.

This center of perception must be by convention the current location of the perceiving instance, as:

 - a given instance should only perceive its own neighborhood
 - the environment will take advantage of this call to update its knowledge of the location of the perceiver
