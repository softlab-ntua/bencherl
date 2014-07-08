:raw-latex:`\pagebreak`

Using Type Specifications With Sim-Diasca
=========================================



Type Specifications: What For?
------------------------------

Adding a type specification (shorthand: *spec*) to the source code of a function means specifying what are its intended input and output parameters, in terms of number and types. This can be applied to records as well.

Once all Sim-Diasca code (including prerequisites, like ``Common``, ``WOOPER`` and ``Traces``) and the one of user applications based on it (ex: ``Mock-Simulators`` or any actual simulator) have been instrumented with type specifications (i.e. when all exported functions and records of all modules have a proper spec), then:

 - static checkings can be done: `Dialyzer <http://www.it.uu.se/research/group/hipe/dialyzer>`_ is able to detect various discrepancies (such as type errors, unreachable code, unnecessary tests, etc.) at compile-time (therefore a lot earlier than at runtime, and allowing to examine *a priori* all code paths)

 - more precise and useful documentation can be generated, thanks to `edoc <http://erlang.org/doc/apps/edoc/users_guide.html>`_


Some versions of Sim-Diasca are referenced in `Dialyzer's application repository <http://dialyzer.softlab.ntua.gr/apps/#Sim-Diasca-2-0-10>`_.



Type Specifications: How?
-------------------------


Prerequisites
.............


Taking Care of Erlang/OTP
_________________________

First of all, type information must have been already extracted from the Erlang/OTP files of the install that will be used, and stored in a PLT (for *Persistent Lookup Table*) file for later-reuse (this is a preprocessing stage). Such a file is preferably created one time for all for each Erlang environment being used. Therefore a PLT file is better produced as the last step of an Erlang installation (see the ``--generate-plt`` option of our ``install-erlang.sh`` script, which streamlines it). This operation is rather long (ex: one hour and a half).


We prefer to have Dialyzer operate on BEAM files (``*.beam``) rather than on source files (``*.hrl/*.erl``), as include paths, symbol definitions and parse transforms are better supported this way.

These BEAM files must have been compiled with debug information (i.e. with the ``+debug_info`` compiler option). This is thus the default enforced for the full Sim-Diasca software stack.

When writing type specifications, one must know what are the built-in ones, in order to re-use them, so that they do not end up being defined more than once, under different names. To do so, one may use our ``common/src/scripts/list-available-types.sh`` script, like in::

  $ cd otp_src_RxBy
  $ list-available-types.sh | tee declared-types-in-Erlang-RxBy.txt



Taking Care of Above Layers
___________________________

Once a PLT is available for Erlang/OTP, PLTs are to be generated for the entire codebase of interest (typically Sim-Diasca and its prerequisites, and possibly user code as well).

This can be achieved with the ``generate-all-plt`` make target, to be run from the root of either a check-out or an install. The script will climb our software stack layer by layer, and generate for each a custom PLT (ex: ``common.plt``).


If, for any reason, the PLT of a layer must be (re)generated, simply use the ``generate-local-plt`` make target from the root of this layer.

The generation of a PLT will notably allow to catch discrepancies of calls with regard to the specs of the functions being called.

Checking specs against the functions they apply to is useful as well. This can be done on any layer through the ``self-check-against-plt`` make target.

Like for Erlang, for each layer a repository of the type declarations defined there can be built, either by running ``make generate-list-of-local-types`` from the root of that layer (producing then a ``declared-types-in-<LAYER>.txt`` file), or by running ``make generate-list-of-all-types`` from  the Sim-Diasca root to have all lists of types generated at once.



Expressing Type Specifications
..............................

The complete syntax is described `here <http://erlang.org/doc/reference_manual/typespec.html#id74368>`_. This will be the main reference to be used and kept ready when writing type specs.


Following conventions are to respect:

 - type specifications must be defined in all source files

 - all exported functions and records defined in headers should have a type spec, and this spec should be specified on the line just before their own definition; local functions and records may or may not have type specs

 - these type specs are to be defined as soon as a new function or record is introduced

 - as soon as a data-structure is being used more than once (ex: let's suppose a timestamp is being defined as a triplet of positive integers), a user-specific type *must* be defined (ex: ``-type MyTimeStamp() :: {pos_integer(),pos_integer(),pos_integer()}.``) and re-used *everywhere applicable* (ex: ``-spec get_timestamp() -> MyTimeStamp().``)

 - all type definitions (opaque or not) must be declared in a relevant module (least astonishment principle), and must be gathered in a section near the top of the file

 - types that may be potentially reused elsewhere must be exported; conversely, relevant types that have been already defined must be reused (instead of being defined multiple times); to know what are the currently known types, use our ``list-available-types.sh`` script

 - type specs should include no extraneous whitespaces and should respect the usual 80 character wide lines (thus possibly being broken into multiple lines)

 - Dialyzer should be run regularly against the codebase to check frequently whether the sources are correct


For a larger codebase to instrument with type specs, it may be useful to start first with the specs that can be deduced by Dialyzer from the actual code of functions. This can be done thanks to our ``add-deduced-type-specs.escript`` script (in ``common/src/scripts``). One should note that these specs are not, in the general case, the ones that the developer would have written (as Dialyzer cannot guess the intent of the original developer), so at least some adaptation work remains (ex: to define reusable types).


.. Note::
   A developer may *overspec* or *underspec*.

   *Overspecification* corresponds to the writing of type specifications that are narrower than the allowed types that a function could process. For example, even if a given function happened to be able to use improper lists as well, the developer may decide that only proper lists are to be passed. Similarly, one may prefer ``string()`` to ``[[any()] | char()]``.

   Reciprocally, *underspecification* corresponds to the writing of type specifications that are larger than the allowed types that a function could process. This may happen if planning to expand later the inputs that a function can take into account.

   Overspecification is perfectly legitimate, whereas underspecification should preferably be avoided.



Checking Type Specifications
............................

In the context of each layer, one may routinely run::

  $ make clean all generate-local-plt

This allows to list all the types that are unknown (generally misspelled or not exported) and spot a few kinds of errors (ex: ``Call to missing or unexported function``).

For a layer ``foo`` (ex: ``Common``, ``WOOPER``, etc.), one should run from its root directory::

  $ make self-check-against-plt


You will have an output like::

 $ make self-check-against-plt
   Building all, in parallel over 8 core(s), from BASE/foo
   [..]
	Checking foo against its PLT (./foo.plt)
  Checking whether the PLT ./foo.plt is up-to-date... yes
  Compiling some key modules to native code... done in 0m29.49s
  Proceeding with analysis...
  bar.erl:53: Function run/0 has no local return
  [..]



Issues can then be tackled one by one. To speed up the process of improving a module ``bar``, one can run::

 $ make bar.plt
 Checking module 'bar.beam' against relevant PLT
 [...]

And only this module will be checked, allowing to fix them one by one.


.. Note:: When a source file is modified, the rebuild the BEAM must be triggered specifically, otherwise Dialyzer will not detect that its PLT is not up-to-date anymore (it relies on the timestamp of the BEAM file, not on the one of the ``*.erl`` file).



References
----------

 - `Dialyzer homepage <http://www.it.uu.se/research/group/hipe/dialyzer>`_
 - `a useful Dialyzer practical guide <http://www.ejabberd.im/dialyzer>`_
 - `Types (or lack thereof) <http://learnyousomeerlang.com/types-or-lack-thereof>`_
 - `Types and Function Specifications <http://erlang.org/doc/reference_manual/typespec.html>`_
 - `edoc User's Guide <http://erlang.org/doc/apps/edoc/users_guide.html>`_
