.. comment ::raw-latex:`\pagebreak`


------------------------------------------
Sim-Diasca Management of Simulation Inputs
------------------------------------------


Principles
==========

A simulation is defined primarily by its simulation case, which shall specify the simulation inputs.

These inputs are mostly:

 - the **technical settings**, like the various simulation, deployment and load balancing settings (see the corresponding records to properly specify these inputs)

 - the **initial state** of the simulation, i.e. what are the initial actors and scenarios, and in which state they shall be when the simulation begins



Requirements
============

The initial state of the simulation is by far the most challenging input to manage. In simple cases, it can be seen as a series of (synchronous) creations of initial instances (actors or scenarios, no difference from the engine's point of view), which can be done:

 - either *programmatically* - directly from the simulation case or from a scenario (in both cases typically thanks to a series of ``class_Actor:create_initial_actor/{2,3}`` calls, with the relevant parameters in terms of model construction)
 - or based on *initialisation data*, read from any kind of information stream (typically user-specified initialisation files)


Initialisation may not be straightforward, notably because actors may have to know others, forming a directed graph that may comprise cycles.

A simple example would be when two actors (A and B) have to know each other initially. This initial state cannot be created in one pass as, when the first actor is created (let's say: A), the second (B) does not even exist yet - so A will have to be created as if it were alone, and must be notified afterwards that it is to know B - once B will have been created as well [#]_.

.. [#] B then could be fully created in one pass, since A is existing at this point. However, to preserve symmetry and homogeneity, and avoid being associated to a A instance that is partly initialised (it does not know B yet), probably B should be created in two passes, exactly as A.

Furthermore, in order to designate instances, most simulation projects tend to rely on identifier conventions of their own, that are specific to these simulations and may be of approximately any datatype.

For example some projects will rely on numerical instance identifiers (ex: ``1275411``), other may use names (ex: ``1600 Pennsylvania Ave NW, Washington, DC 2050012, United State``) while some simulations could rely on more complex data structures (ex: ``(12,3.0,45)``, whatever this may mean).

Qualities of an input management system include:

 - allowing most kinds of user-specific instance identifiers
 - managing its own, private identifiers transparently for the user
 - being able to sort out all cyclic dependencies
 - not preventing at least some sort of efficient distributed mode of operation afterwards, notably with regard to smart placement and load-balancing
 - relying on an expressive, yet compact and human-readable, form to specify the initial instances
 - managing this initialisation data as much as possible in parallel



Creation of the Initial State of the Simulation
===============================================


How to Create Initial Instances Programmatically
------------------------------------------------

This is the easiest, yet most limited, approach. No external identifier needs to be involved here.

If wanting to create two instances that have to know each other, one may use, possibly directly from the simulation case::

 A = class_Actor:create_initial_actor( class_Foo, [ Xa, Ya ] ),
 B = class_Actor:create_initial_actor( class_Bar, [ Xb, Yb, Zb ],
	"My placement hint" ),

 A ! { declareBar, B, self() },
 B ! { declareFoo, A, self() },

 bar_declared = test_receive(),
 foo_declared = test_receive(),

 [...]


A few remarks here:

 - ``A`` and ``B`` are PIDs
 - this mutual recognition requires a specific method to be added in both classes (namely ``declare{Foo,Bar}/2``)
 - one can guess that the ``declare{Foo,Bar}/2`` methods exposed here are requests (even if no particular result had to be returned), for synchronicity reasons (otherwise there could be a race condition between these messages and the ones related to the start of the simulation); this is why we have to receive the ``*_declared`` messages from the simulation case
 - we can see that here A will be created using default placement policy, when B will be created by the load balancer according to the specified hint: when created programmatically, all instances specifying the same placement hint will be created on the same computing node (as chosen by the load balancer)




How to Use Initialisation Data to Create Initial Actors
-------------------------------------------------------



Specifying the Data Source
..........................

The user shall call, either directly from the simulation case or from a scenario:

 ``sim_diasca:create_initial_instances/1`` [#]_

.. [#] We use *instances*, as they may be actors or scenarios.

or use the ``initialisation_files`` field of the ``simulation_settings`` (see ``soda_loading_test.erl`` for an example).

In both cases we expect a list of filenames to be specified, each being a path (absolute, or relative to the directory from which Sim-Diasca was launched) to a file containing initialisation data of interest.

.. comment to be checked once implemented: Note that if multiple files are specified, they will be processed in parallel, breaking the reproducibility of the execution of that simulation case.

Each of them should be a text file (whose name is arbitrary, but we recommend using the ``.init`` file extension; for example: ``my-case-instances.init``), containing a series of lines, either:

 - blank
 - or containing a comment
 - or containing a creation specification, ending with a dot

Any line may have any number of leading and/or trailing whitespaces.

Each non-empty line that is not a comment is to create an instance, hence shall specify the class name and actual construction parameters that correspond to this instance.

See the ``soda-instances.init`` file (in the ``soda-test`` mock simulator) for a full example.




Regarding Actor Identifiers
...........................


To better integrate into most architectures, Sim-Diasca manages two kinds of identifiers for actor instances created from data:

 - **external** ones, i.e. the arbitrary identifiers that are provided by the user, which are often simulation-specific
 - **internal** ones, i.e. identifiers that are managed internally by the engine, and which are mostly transparent for the user


External identifiers can be arbitrary strings, which are processed as are (no attempt of checking, parsing or enforcing any convention on their content is made there) [#]_.

.. [#] We could even imagine that these identifiers be of any type, however this would offer little practical interest.

The internal identifiers are simply the PID of the corresponding instances.

Thus the engine takes care of letting the user rely on any convention, while maintaining a two-way translation scheme to benefit from the best of both worlds.




Format of a Line for Basic Creation
...................................

Such a line is made of a pair, whose first element is the class (as an atom) of the instance to create and whose second element is a list containing its construction parameters, that may be approximately any Erlang terms [#]_.

.. [#] We will see below that actually only tuples whose first element is the ``user_id`` atom are not accepted as actual initialisation data, since, in this context, they would be ambiguous.


A simple line, designated as a "creation clause", could then be::

 {class_Foo,["Hello world!",1.4]}.


One can see this data-based initialisation as a simple counterpart to this programmatic form::

 class_Actor:create_initial_actor(class_Foo,["Hello world!",1.4])


Such a data-based initialisation allows expressing all creations of initial instances - except the ones that start interlinked and thus that must rely on some sort of (user-defined) instance identifiers.

A basic creation can also be performed with an additional parameter, which is a placement hint (which can be any term). This tells the load balancer to create all instances that are specified with the same placement hint on the same computing node.

Such a creation clause can then be, if using an atom as hint::

 {class_Foo,["Hello world!",1.4],my_placement_hint}.


The corresponding programmatic form being then::

 class_Actor:create_initial_placed_actor(class_Foo,
   ["Hello world!",1.4],my_placement_hint)




Format of a Line *Specifying* a User Identifier
...............................................

The following syntax allows, in addition to the aforementioned creation, to define and associate a specific user-provided identifier to that newly created instance.

We can see that the same basic creation pair as before is now prefixed by its user identifier and an arrow::

 "My first instance" <- {class_Foo,["Hello world!",1.4]}.

As a consequence, the engine will see the ``"My first instance"`` string as a user identifier associated to the PID of the corresponding ``class_Foo`` initial instance that will be created.

The user identifiers are arbitrary strings, except that they should not contain any double quote (``"``) character (to simplify their parsing).

For the engine, *defining* a user identifier results in selecting a related placement of the upcoming instance. Hence no placement hint can be specified with this form.

Of course defining identifiers would be useless if they could not be used afterwards.



Format of a Line *Making Use of* a User Identifier
..................................................

Such a line would be for example::

 {class_Bar,[an_atom,3,{user_id,"My first instance"},7]}.

We can see here that the user identifier previously defined for the ``class_Foo`` instance (i.e. ``My first instance``) will be used in order to create the ``class_Bar`` instance, so that the latter can know the former (i.e. have its PID) from its start (on its creation).

When referenced (as opposed to being defined), user identifiers are to be tagged thanks to a ``user_id`` pair. For example ``{user_id,"My first instance"}`` is to be specified, instead of a mere ``"My first instance"`` (which would be interpreted as any random string).

Otherwise simple parameter strings and user identifiers could not be discriminated properly; the ``user_id`` atom is thus reserved for such use.

No user identifier being *defined* here, a placement hint can also be specified. For example as a string (here, "Milky Way")::

  {class_Dalek,[true,{user_id,"EXTERMINATE"}],"Milky Way"}.



Format of a Line in the General Case
....................................

Often a given instance will reference some others (i.e. rely on their user identifier) *and* have its own user identifier defined, like in::

 "John" <- {class_Beatle,[{user_id,"Paul"},{user_id,"George"}]}.

Here John will know from the start Paul and George, and later in the initialisation phase any Ringo could know John as well, using ``{user_id,"John"}`` for that.

As always, a user identifier being defined here, no placement hint can be specified.



More Information About Placement Hints
......................................

We can see that no placement hint could be specified in the creation lines above, as they defined a user identifier.

Indeed, with data-based initialisations, placement derives naturally from user identifiers:

- if a user identifier is specified (ex: ``"My Foo" <- {class_Foo,[...]}``), then this identifier (``"My Foo"``) will be used as a placement hint

- if no user identifier is specified:

  - if a placement hint is specified, then it will be used directly

  - if no placement hint is specified either:

	- if no user identifier is referenced either (ex: ``{class_Foo,["Hello world!",1.4]}.``), then the corresponding instance will be placed according to the default policy of the load balancer

	- if at least one user identifier is referenced (ex: ``{class_Foo,[2,{user_id,"AA"},0.0,{user_id,"BB"}, my_atom]}.``), then the corresponding instance will be placed according to the first user identifier found when parsing the construction parameters; so, in this example, this ``class_Foo`` instance would be created on the same computing node on which the instance designated by user identifier ``"AA"`` will be

This allows an automatic, implicit placement of instances which by design are likely to interact.



Comments
........

An initialisation file may also contain comments. They have to be on a dedicated line, starting with the character ``%``. Then the full line is ignored.



Empty Lines
...........

There are ignored.



Inner Workings Explained
........................

The initialisation data is read and, in parallel, is parsed and checked by a set of creator processes (one per core of the user host).

One instance is created per read creation line (provided it is neither blank nor a comment), and the engine ensures that a hosting process is available for each instance *referenced* in that creation line: any user identifier referenced before being defined will result in a blank process being spawned on the relevant computing node (determined solely from this user identifier); this process will embody the corresponding instance, once its definition will be processed.

The PID of each of these created processes is recorded in a translation table, so that user identifiers can be related to these processes.

Despite the arbitrary creation order induced by parallelism, the engine takes care of assigning reproducible AAIs and random seeds.

In the meantime the read initialisation terms are transformed, replacing each ``{user_id,UserIdentifier}`` pair (of course these information can be arbitrarily nested in any kind of data-structure, discovered at runtime) by the corresponding PID (that is either already pre-spawned or created at this moment), and the corresponding instances are initialised (their constructor being called with the relevant, transformed construction parameters).

Each user identifier must be defined exactly once; any user identifier:

 - referenced to, but never defined, results in an error
 - defined more than once results in an error

A user identifier that is defined but never referenced is not considered as an error.

When the parsing of a creation line fails, a detailed context is given (with the faulty line verbatim, the file name and line number, and an interpretation of the error).

The `JSON syntax <https://en.wikipedia.org/wiki/JSON>`_ could have been used here (for example relying on `jiffy <https://github.com/davisp/jiffy>`_ or on `jsx <https://github.com/talentdeficit/jsx>`_), but it would not be nearly as compact and adequate as the custom syntax proposed here.



Model Initialisation
....................

One must understand that the indirection level provided by user identifiers allows the engine to create initial instances in any order (regardless on any potentially cyclic dependency), thus at full speed, in parallel, with no possible deadlock and while preserving total reproducibility.

This system is designed not to add any constraint onto the actors or scenarios; this however implies that, once a given instance is constructed, any other instance it references (through ``user_id``) may or may not be already constructed; nevertheless its PID is already available and given to the referencing instance, and thus once constructed it will be able to answer any pending message(s) transparently.

The model developer of course should ensure that the deadlocks spared by this instance creation system are not re-introduced by their initialisation logic.

This should not be a real problem, as the trickiest issue, the exchange of references, is already solved by design here.
