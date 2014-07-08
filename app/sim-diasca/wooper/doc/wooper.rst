.. _Top:


.. stylesheet ../../../common/css/Ceylan.css


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




======
WOOPER
======

---------------------------------------------------
*Wrapper for Object-Oriented Programming in Erlang*
---------------------------------------------------


.. Note::

  Most notes are directed to Ulf Wiger (ETC) to help defining how the newer WOOPER version is expected to behave. Any remark, feedback, suggestion is welcome!



.. Note::

  I will update links and versions later.



:raw-html:`<p>The WOOPER documentation is also available in the PDF format: see <a href="http://ceylan.sourceforge.net/main/documentation/wooper/wooper.pdf">wooper.pdf</a>.</p>`

:raw-latex:`This documentation is also available directly from the \href{http://ceylan.sourceforge.net/main/documentation/wooper/index.html}{WOOPER homepage}.`


Latest stable WOOPER archives are:

 - `wooper-2.0.tar.bz2 <http://downloads.sourceforge.net/ceylan/wooper-2.0.tar.bz2>`_

 - `wooper-2.0.zip <http://downloads.sourceforge.net/ceylan/wooper-2.0.zip>`_




.. _`table of contents`:

.. contents:: Table of Contents
.. section-numbering::


:raw-latex:`\pagebreak`


Overview
========

WOOPER, which stands for *Wrapper for Object-Oriented Programming in Erlang*, is an `open source`_ lightweight layer on top of the `Erlang <http://erlang.org>`_ language, which provides constructs dedicated to `Object-Oriented Programming <http://en.wikipedia.org/wiki/Object-oriented_programming>`_ (OOP).

WOOPER is a (completely autonomous) part of the `Ceylan <http://ceylan.sourceforge.net>`_ project.



Motivations & Purpose
---------------------

Some problems may almost only be tackled efficiently thanks to an object-oriented modelling.

The set of code and conventions proposed here allows to benefit from all the main OOP features (including polymorphism, life cycle management, state management and multiple inheritance) directly from Erlang (which natively does not rely on the OOP paradigm), so that an object-oriented approach at the implementation level can be easily achieved, in the cases where it makes sense.




WOOPER Mode of Operation In A Nutshell
--------------------------------------

The WOOPER OOP concepts translate into Erlang constructs according to the following mapping:

  ======================  =================================================================
  WOOPER concept          Corresponding Erlang mapping
  ======================  =================================================================
  class definition        module
  instance                process
  instance reference      process identifier (PID)
  new operators           WOOPER-provided functions, making use of user-defined ``construct/N`` functions (a.k.a. the constructors)
  delete operators        WOOPER-provided functions, unless user-specified (a.k.a. the destructor)
  method definition       module function that respects some conventions
  method invocation       sending of an appropriate inter-process message
  method look-up          class-specific virtual table taking into account inheritance transparently
  instance state          instance-specific datastructure, kept by the instance-specific WOOPER tail-recursive infinite loop
  instance attributes     key/value pairs stored in the instance state
  class (static) method   exported module function
  ======================  =================================================================

In practice, developing a class with WOOPER just involves including the `wooper.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper.hrl?view=markup>`_ header file and respecting the WOOPER conventions detailed below.




:raw-latex:`\pagebreak`


Example
-------

.. Note::

	I suppose requesting the developer to specify the class-specific information (like superclasses, attributes, etc.) is probably best done thanks to dedicated functions (like ``get_superclasses() -> [class_X,classY]``) rather than thanks to macro defines (like ``-define(wooper_superclasses,[class_X,classY]``)?

	Construction parameters are to be directly determined from the ``construct/N`` definition (no more ``wooper_construct_parameters`` define). From their deduced number, the ``wooper_construct_export`` define (ex: with ``new/4, new_link/4, synchronous_new/4, .., remote_new/5, ..`` can now be automatically declared, and methods be exported.

	Parse transforms are probably the solution for both needs.

	Attributes may be declared with qualifiers (public, protected or private, const, final) even if the current WOOPER version does not enforce all qualifiers in all cases.

	Functions for state management are now functions (expected to be inlined): ``?setAttribute(..)`` becomes ``setAttribute(..)``, to avoid macro pitfalls (see comments in previous ``wooper.hrl``).

	In all cases, state management is to deal with class-specific dedicated records (i.e. for each class a record listing all its attributes, inherited or not, is specifically defined at compile-time (by gathering all attributes from the get_attributes/0 functions in the inheritance graph), and a state variable is then merely a record instance. An attribute should be only defined once, i.e. if class A defines attribute ``my_attribute``, then no direct or indirect child class of A can define an attribute ``my_attribute`` (compile-time error).

	It would be convenient if for each class three static methods where automatically defined, ``get_all_mother_classes/0``, ``get_all_attributes/0`` and ``get_all_methods/0``, which would list their elements while taking into account all mother classes, direct or not.


Here is a simple example of how WOOPER instances can be managed. This shows ``new/delete`` operators, method calling (both request and oneway), and inheritance (a cat is here a viviparous mammal, as defined in the example_ below)::

  -module(class_Cat).

  % Allows to define WOOPER base variables and methods for that class:
  -include("wooper.hrl").

  % Determines what are the mother classes of this class (if any):
  get_superclasses() ->
	[class_Mammal,class_ViviparousBeing].

  % Determines what are the class-specific attributes of this class (if any):
  get_attributes() ->
  	[ {paw_color,protected}, whisker_color, {mew_volume,[private,{const,35}]} ].

  % Determines what are the static methods of this class (if any):
  get_static_methods() ->
	[ {get_default_whisker_color,0}, {compute_mew_frequency,2} ].

  % Determines what are the class-specific member methods of this class
  % (if any):
  get_member_methods() ->
  	[ {getMewVolume,1}, {canEat,2,[public,final]},
	  {getWhiskerColor,1,public}, {setWhiskerColor,2,protected} ].

  % Constructs a new Cat.
  construct(State,Age,Gender,FurColor,WhiskerColor) ->
  	% First the direct mother classes:
  	MammalState = class_Mammal:construct(State,Age,Gender,FurColor),
  	ViviparousMammalState = class_ViviparousBeing:construct(MammalState),
  	% Then the class-specific attributes:
  	setAttributes(ViviparousMammalState,whisker_color,WhiskerColor ).

  % Static method:
  get_default_whisker_color() ->
  	white.

  % Returns how loud this cat can mew.
  % (const request; could be a static method as well)
  getMewVolume(State) ->
  	?wooper_return_state_result( State, 6 ).

  % Overrides any request method defined in the Mammal class:
  % (const request)
  canEat(State,soup) ->
  	?wooper_return_state_result( State, true );

  canEat(State,croquette) ->
  	?wooper_return_state_result( State, true );

  canEat(State,meat) ->
  	?wooper_return_state_result( State, true );

  canEat(State,_) ->
  	?wooper_return_state_result( State, false ).

  % Another cat-specific const request:
  getWhiskerColor(State)->
  	?wooper_return_state_result( State, ?getAttr(whisker_color) ).

  % A (non-const) oneway:
  setWhiskerColor(State,NewColor)->
  	NewState = setAttribute( State, whisker_color, NewColor ),
  	?wooper_return_state_only( NewState ).


Straightforward, isn't it? We will discuss it in-depth though.

To test this class, just extract your WOOPER archive, like in::

  tar xvjf wooper-x.y.tar.bz2
  cd wooper-x.y

and run, from the root of this archive (the ``wooper-x.y`` directory)::

  make all && cd wooper/examples && make class_Cat_run


Then, in the ``examples`` directory, the test defined in ``class_Cat_test.erl`` should run against the class defined in ``class_Cat.erl``; no error should be detected::

  [..]
  Deleting cat <0.41.0>! (overridden destructor)
  Deleting mammal <0.41.0>! (overridden destructor)
  --> This cat could be created and be synchronously deleted, as expected.
  --> End of test for module class_Cat.



:raw-latex:`\pagebreak`

Why Adding Object-Oriented Capabilities To Erlang?
==================================================

Although applying blindly OOP while using languages based on other paradigms (Erlang ones are functional and concurrent, the language is not specifically targeting OOP) is a common mistake, there are some problems that may be deemed inherently "object-oriented", i.e. that cannot be effectively modelled without encapsulated abstractions sharing behaviours.

Examples of this kind of systems are multi-agent simulations. If they often need massive concurrency, robustness, distribution, etc. (Erlang is particularly suitable for that), the various actor types have also often to share numerous states and behaviours, while still being able to be further specialised on a per-type basis.

The example_ chosen here is a simulation of the interacting lives of numerous animals from various species. Obviously, they have to share behaviours (ex: all ovoviviparous beings may lay eggs, all creatures can live and die, all have an age, etc.), which cannot be mapped easily (read: automatically) to Erlang concepts without adding some generic constructs.

WOOPER, which stands for *Wrapper for OOP in Erlang*, is a lightweight yet effective (performance-wise, but also regarding the overall developing efforts) means of making these constructs available, notably in terms of state management and multiple inheritance.

The same programs could be implemented without such OOP constructs, but at the expense of way too much manually-crafted specific (per-class) code. This process would be tedious, error-prone, and most often the result could hardly be maintained.





:raw-latex:`\pagebreak`

How to Use WOOPER: Detailed Description & Concept Mappings
==========================================================


.. contents::
 :local:
 :depth: 2


.. Note::

  We will discuss here mostly of the WOOPER versions 2.x and higher, development branch which is sometimes codenamed the "*Zero-Overhead WOOPER*", as opposed to the legacy versions (prior to 2.x), codenamed "*Hashtable-Based WOOPER*".


Classes
-------


Class & Names
.............

A class is a blueprint to create objects, a common scheme describing the behaviour and the internal data types of its instances, i.e. the attributes and methods that the created objects for that class all share.

With WOOPER each class must have a unique name.

To allow for **encapsulation**, a WOOPER class is mapped to an Erlang module, whose name is by convention made from the ``class_`` prefix followed by the class name, in the so-called `CamelCase <http://en.wikipedia.org/wiki/CamelCase>`_: all words are spelled in lower-case except their first letter, and there are no separators between words, like in: *ThisIsAnExample*.

For example, a class modeling a cat should translate into an Erlang module named ``class_Cat``, thus in a file named ``class_Cat.erl``. At the top of this file, the corresponding module would be therefore declared with: ``-module(class_Cat).``.

Similarly, a pink flamingo class could be declared as ``class_PinkFlamingo``, in ``class_PinkFlamingo.erl``, which would include a ``-module(class_PinkFlamingo).`` declaration.


The class name can be obtained through its ``get_class_name`` WOOPER-defined static method [#]_::

  > class_Cat:get_class_name().
  class_Cat

.. [#] The ``get_class_name`` static method has no real interest, it is defined mostly for teaching purpose.

Note that a static method (i.e. a class method that does not apply to any specific instance) of a class X is nothing more than an Erlang function exported from the corresponding ``class_X`` module: all exported functions can be seen as static methods.



Inheritance & Superclasses
..........................

A WOOPER class can inherit from other classes, in this case the behaviour and the internal data defined in the mother classes are available by default to this child class.

Being in a **multiple inheritance** context, a given class can have any number (``[0..n]``) of direct mother classes, which themselves may have mother classes, and so on.

This is declared in WOOPER thanks to the ``get_superclasses/0`` function. For example, a class with no mother class should specify, once having declared its module, ``get_superclasses() -> [].`` [#]_.

.. [#] Such WOOPER-related functions are already automatically exported by WOOPER. As an added bonus, this allows the class developer to be notified whenever he forgets to define them.

As for our cat, this animal could be modelled both as a mammal (itself a specialised creature) and a viviparous being [#]_. Hence its direct inheritance could be defined as: ``get_superclasses() -> [class_Mammal, class_ViviparousBeing].``.

.. [#] Neither of them is a subset of the other, these are mostly unrelated concepts, at least in the context of that example! (ex: a platypus is a mammal, but not a viviparous being).

The superclasses (direct mother classes) of a given class can be known thanks to its ``get_superclasses`` static method::

  > class_Cat:get_superclasses().
  [class_Mammal,class_ViviparousBeing]



Instances
---------


Instance Mapping
................

With WOOPER, which focuses on multi-agent systems, all **instances** of a class are mapped to Erlang processes (one WOOPER instance is exactly one Erlang process).

They are therefore, in UML language, *active objects* (each has its own thread of execution, they may apparently "live" simultaneously).


Instance State
..............

Another need is to rely on **state management** and **encapsulation**: each instance should be stateful, have its state private, and be able to inherit automatically the data members defined by its mother classes.

In WOOPER, this is obtained thanks to a per-instance associative table, whose keys are the names of attributes and whose values are the attribute values. This will be detailed in the `state management`_ section.




:raw-latex:`\pagebreak`


Methods
-------

They can be either:

 - *member methods*: applied to a specific class instance, like ``MyCat ! declareBirthday``
 - or *static methods*: general to a class, not targeting specifically an instance, like ``class_Cat:get_default_mew_duration()``

Unless specified otherwise, just mentioning *method* by itself refers to a *member method*. Static methods are discussed into their specific subsection.

Instances may declare **member methods** that can be publicly called, whether locally or remotely (i.e. on other networked computers, like with RMI or with CORBA, or directly from the same Erlang node). Distribution is seamlessly managed thanks to Erlang.

Member ethods (either inherited or defined directly in the class) are mapped to specific Erlang functions, triggered by Erlang messages.

For example, our cat may define, among others, following member methods:

 - ``canEat``, taking one parameter specifying the type of food, and returning whether the cat can eat that kind of food. The implementation should be cat-specific here, whereas the method signature is shared by all beings

 - ``getWhiskersColor``, taking no parameter, returning the color of its whiskers. This is indeed a purely cat-specific method

 - ``declareBirthday``, incrementing the age of our cat, not taking any parameter nor returning anything. It will be therefore be implemented as a oneway call (i.e. not returning any result to the caller, hence not even needing to know it), whose call is only interesting for its effect on the cat state: here, making it one year older

 - ``setWhiskerColor``, assigning the specified color to the whiskers of that cat instance

Declaring a birthday is not cat-specific, nor mammal-specific: we can consider it being creature-specific. Cat instances should then inherit this method, preferably indirectly from the ``class_Creature`` class, in all cases without having to specify anything, since the information given by ``get_superclasses/0`` tells it already. However this inherited method can be overridden at will anywhere in the class hierarchy.


We will discuss the *definition* of these methods later, but for the moment let's determine their signatures and declarations, and how we are expected to *call* them.



Method Declaration
..................

The cat-specific member (i.e. non-static) methods are to be declared:

 - in the ``class_Cat``
 - thanks to the ``get_member_methods/0`` function, which automatically exports them

Their arity should be equal to the number of parameters they should be called with, plus one.

The additional parameter is an implicit one (automatically managed by WOOPER), corresponding to the state of the instance.

This ``State`` variable defined by WOOPER can be somehow compared to the ``self`` parameter of Python, or to the ``this`` hidden pointer of C++. That state is automatically kept by WOOPER instances in their main loop, and automatically prepended to the parameters of incoming method calls.

In our example, the declarations could therefore result in::

  get_member_methods() ->
	[ {getMewVolume,1}, {canEat,2, [public,final]},
	  {getWhiskerColor,1,[public,const]}, {setWhiskerColor,2,protected} ].


More generally a method can be declared with:

 - just its name and full arity (including the ``State`` parameter), ex: ``{getMewVolume,1}``
 - its name, full arity, and one qualifier, ex: ``{getWhiskerColor,1,public}``
 - its name, full arity, and a list of qualifiers, ex: ``{canEat,2, [public,final]}``


Known method qualifiers are:

 - in terms of accessibility:

   - ``public``: the method can be called from outside the instance as well from the class itself, i.e. from the body of its own methods (inherited or not), or from its child classes
   - ``protected``: the method can be called only from the body of its own methods (inherited or not), or from its child classes; no call from outside the class
   - ``private``: the method can be called only from the body of its own methods (inherited or not); no call from outside the class or from child classes is allowed

 - in terms of mutability:

   - ``const``: a call of the method on an instance will then never result into a change in the state of that instance

   - ``final``: this method cannot be overridden by child classes

Unless specified otherwise, a method is public, non-const, non-final.


.. Note::

  WOOPER allows to *specify* these qualifiers for documentation purposes, but may or may not enforce them.

  For example, to anticipate a bit, all methods could be dispatched into three lists (for public/protected/private), and when an ``execute*`` call is performed, a check, based on the actual class of the instance, could be done.

  On the other hand, method calls, triggered by messages instead, could not have their access controlled (without even mentioning the runtime overhead). For example, protected oneways cannot be checked for accessibility, as the message sender is not known in the context of this kind of method call.


As ``declareBirthday`` will be inherited but not overridden, no need to declare it.

Some method names are reserved for WOOPER: no user method should have a name starting by ``wooper``.

The complete list of reserved function names that do not start with the ``wooper_`` prefix is:

 - ``get_class_name``
 - ``get_superclasses``
 - ``executeRequest``
 - ``executeOneway``
 - ``delete_any_instance_referenced_in``
 - ``is_wooper_debug``

They are reserved for all arities.

Note that functions which must be defined by the class developer are unconditionally exported by the WOOPER header, so that a compile-time error is issued whenever at least one of them is not defined.



Method Invocation
.................

Let's suppose that the ``MyCat`` variable designates an instance of ``class_Cat``. Then this ``MyCat`` reference is actually just the PID of the Erlang process corresponding to this instance.

All methods, either defined directly by the actual class or inherited, are to be called from outside this class thanks to a proper Erlang message, sent to the instance PID.

When the caller needs a result to be sent back, it must specify to the instance its own PID (i.e. the caller PID), so that the instance knows to whom the answer should be sent.

Therefore the ``self()`` parameter in the call tuples below corresponds to the PID *of the caller*: ``MyCat`` is the PID of the target instance.

The three methods previously discussed would indeed be called that way::

  % Calling the canEat request of our cat instance:
  MyCat ! {canEat,soup,self()},
  receive
	  {wooper_result,true} ->
			   io:format( "This cat likes soup!!!" );

	  {wooper_result,false} ->
			   io:format( "This cat does not seem omnivorous." )
  end,

  % A parameter-less request:
  MyCat ! {getWhiskersColor,[],self()},
  receive
	  {wooper_result,white} ->
			   io:format( "This cat has normal whiskers." );

	  {wooper_result,blue} ->
			   io:format( "What a weird cat..." )
  end,

  % A parameter-less oneway:
  MyCat ! declareBirthday.



Method Name
...........

Methods are designated by their atom name, as declared in the ``get_member_methods/0`` function of the class in the inheritance tree that defines them.

The method name is always the first information given in the method call tuple.



Method Parameters
.................

As detailed below, there are:

 - *requests* methods: they perform some processing and then return a result to the caller (obviously they need to know it, i.e. the caller have to specify its PID)

 - *oneway* methods: they only change the state of the instance, with no reply being sent back (no caller PID to specify)

Both can take any number of parameters, including none. The **marshalling** of these parameters and, if relevant, of returned values is performed automatically by Erlang.

Parameters are to be specified in a (possibly empty) list, as second element of the call tuple.

If only one parameter is needed, the list can be omitted, and the parameter can be directly specified: ``Me ! {setAge,31}.`` works just as well as ``Me ! {setAge,[31]}.``.


.. _`single method parameter is a list`:

.. Note::
  This cannot apply if the unique parameter is a list, as this would be ambiguous.

  For example: ``Foods = [meat,soup,croquette], MyCat ! {setFavoriteFoods,Foods}`` would result in a call to ``setFavoriteFoods/4``, i.e. a call to ``setFavoriteFoods(State,meat,soup,croquette)``, whereas the intent of the programmer is probably to call a ``setFavoriteFoods/2`` method like ``setFavoriteFoods(State,Foods) when is_list(Foods) -> [..]``.

  The proper call would then be ``MyCat ! {setFavoriteFoods,[Foods]}``, i.e. the parameter list should be used, it would then contain only one element, the food list, whose content would therefore be doubly enclosed.



Two Kinds of Methods
....................


Request Methods
_______________

A **request** is a method that returns a result to the caller.

For an instance to be able to send an answer to a request triggered by a caller, of course that instance needs to know the caller PID.

Therefore requests have to specify, as the third element of the call tuple, an additional information: the PID to which the answer should be sent, which is almost always the caller (hence the ``self()`` in the actual calls).

So these three potential information (request name, parameters, reference of the sender, i.e. an atom, usually a list, and a PID) are gathered in a tuple sent as a message: ``{request_name,[Arg1,Arg2,..],self()}``.

If only one parameter is to be sent, and if that parameter is not a list, then this can become ``{request_name,Arg,self()}``.

For example: ``MyCat ! {getAge,[],self()}`` or ``MyCalculator ! {sum,[1,2,4],self()}``.

The actual result ``R``, as determined by the method, is sent back as an Erlang message which is a ``{wooper_result,R}`` pair, to help the caller pattern-matching the messages in its mailbox.

``receive`` should then be used by the caller to retrieve the request result, like in the case of this example of a 2D point instance::

 MyPoint ! {getCoordinates,[],self()},
 receive
 		  {wooper_result,[X,Y]} ->
 		 		  [..];
 		  % Could be left out to ignore errors.
 		  % Otherwise one might prefer making this caller block:
 		  Error ->
 		 		  [..]
 end,
 [..]



Oneway Methods
______________

A **oneway** is a method that does not return a result to the caller.

When calling oneway methods, the caller does not have to specify its PID, as no result is expected to be returned back to it.

If ever the caller sends by mistake its PID nevertheless, a warning would be sent back to it, the atom ``wooper_method_returns_void`` instead of ``{wooper_result,Result}``.

The proper way of calling a oneway method is to send to it an Erlang message  that is:

 - either a pair, i.e. a 2-element tuple (therefore with no PID specified): ``{oneway_name,[Arg1,Arg2,..]}`` or ``{oneway_name,Arg}`` if ``Arg`` is not a list. For example: ``MyPoint ! {setCoordinates,[14,6]}`` or ``MyCat ! {setAge,5}``

 - or, if the oneway does not take any parameter, just the atom ``oneway_name``. For example: ``MyCat ! declareBirthday``


No return should be expected (the called instance does not even know the PID of the caller), so no receive should be attempted on the caller side, unless wanting to wait until the end of time.

Due to the nature of oneways, if an error occurs instance-side during the call, the caller will never be notified of it.

However, to help the debugging, an error message is then logged (using ``error_logger:error_msg``) and the actual error message, the one that would be sent back to the caller if the method was a request, is given to ``erlang:exit`` instead.




Method Results
..............



Execution Success: ``{wooper_result,ActualResult}``
___________________________________________________

If the execution of a method succeeded, and if it is a request (not a oneway, which would not return anything), then ``{wooper_result,ActualResult}`` will be sent back.

Otherwise one of the following error messages will be emitted.



Execution Failures
__________________


When the execution of a method fails, three main error results can be returned.

A summary could be:

+-----------------------------------+----------------------------+------------------+
| Error Result                      | Interpretation             | Guilty           |
+===================================+============================+==================+
| ``wooper_method_not_found``       | No such method exists in   | Caller           |
|                                   | the target class.          |                  |
+-----------------------------------+----------------------------+------------------+
| ``wooper_method_failed``          | Method triggered a runtime | Called instance  |
|                                   | error (it has a bug).      |                  |
+-----------------------------------+----------------------------+------------------+
| ``wooper_method_faulty_return``   | Method does not respect    | Called instance  |
|                                   | the WOOPER return          |                  |
|                                   | convention.                |                  |
+-----------------------------------+----------------------------+------------------+



``wooper_method_not_found``
***************************

The corresponding error message is ``{wooper_method_not_found, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters}``.

For example ``{wooper_method_not_found, <0.30.0>, class_Cat, layEggs, 2, ...}``.

Note that ``MethodArity`` counts the implied state parameter (that will be discussed later), i.e. here ``layEggs/2`` might be defined as ``layEggs(State,NumberOfNewEggs) -> [..]``.

This error occurs whenever a called method could not be found in the whole inheritance graph of the target class. It means this method is not implemented, at least not with the deduced arity.

More precisely, when a message ``{method_name,[Arg1,Arg2,..,Argn]...}`` (request or oneway) is received, ``method_name/n+1`` has be to called: WOOPER tries to find ``method_name(State,Arg1,..,Argn)``, and the method name and arity must match.

If no method could be found, the ``wooper_method_not_found`` atom is returned (if the method is a request, otherwise the error is logged), and the object state will not change, nor the instance will crash, as this error is deemed a caller-side one (i.e. the instance has a priori nothing to do with the error).



``wooper_method_failed``
************************

The corresponding error message is ``{wooper_method_failed, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters, ErrorTerm}``.

For example, ``{wooper_method_failed, <0.30.0>, class_Cat, myCrashingMethod, 1, [], {{badmatch,create_bug}, [..]]}``.

If the exit message sent by the method specifies a PID, it is prepended to ErrorTerm.

Such a method error means there is a runtime failure, it is generally deemed a instance-side issue (the caller should not be responsible for it, unless it sent incorrect parameters), thus the instance process logs that error, sends an error term to the caller (if and only if it is a request), and then exits with the same error term.



``wooper_method_faulty_return``
*******************************

The corresponding error message is ``{wooper_method_faulty_return, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters, ActualReturn}``.

For example, ``{wooper_method_faulty_return, <0.30.0>, class_Cat, myFaultyMethod, 1, [], [{{state_holder,..]}``.

This error occurs only when being in debug mode.

The main reason for this to happen is when debug mode is set and when a method implementation did not respect the expected method return convention (neither ``wooper_return_state_result`` nor ``wooper_return_state_only`` was used in this branch of the method definition).

It means the method is not implemented correctly (it has a bug), or that it was not (re)compiled with the proper debug mode, i.e. the one the caller was compiled with.

This is an instance-side failure (the caller has no responsibility for that), thus the instance process logs that error, sends an error term to the caller (if and only if it is a request), and then exits with the same error term.



Caller-Side Error Management
****************************

As we can see, errors can be better discriminated if needed, on the caller side.
Therefore one could make use of that information, as in::

  MyPoint ! {getCoordinates,[],self()},
  receive
	  {wooper_result, [X,Y] } ->
			   [..];
	  {wooper_method_not_found, Pid, Class, Method, Arity, Params} ->
			   [..];
	  {wooper_method_failed, Pid, Class, Method, Arity, Params, ErrorTerm} ->
			   [..];
	  % Error term can be a tuple {Pid,Error} as well, depending on the exit:
	  {wooper_method_failed, Pid, Class, Method, Arity, Params, {Pid,Error}} ->
			   [..];
	  {wooper_method_faulty_return, Pid, Class, Method, Arity, Params, UnexpectedTerm} ->
			   [..];
	  wooper_method_returns_void ->
			   [..];
	  OtherError ->
			   % Should never happen:
			   [..]
  end.


However defensive development is not really favoured in Erlang, one may let the caller crash on unexpected return instead. Therefore generally one may rely simply on matching the message sent in case of success [#]_::

  MyPoint ! {getCoordinates,[],self()},
  receive
	  {wooper_result, [X,Y] } ->
			   [..]
  end.

.. [#] Then, in case of failure, the method call will become blocking.




Method Definition
.................

Here we reverse the point of view: instead of **calling** a method, we are in the process of **implementing** a callable one.

A method signature has always for first parameter the state of the instance, for example: ``getAge(State) -> [..]``, or ``getCoordinate(State,Index) -> [..]``.

For the sake of clarity, this variable should preferably always be named ``State``.


A method must always return at least the newer instance state, even if the state did not change.

In this case the initial state parameter is directly returned, as is, like in::

  getWhiskerColor(State) ->
	  ?wooper_return_state_result( State, ?getAttr(whisker_color) ).

State is unchanged here.


Note that when a method "returns" the state of the instance, it returns it to the (local, process-wise) private WOOPER-based main loop of that instance: in other words, the state variable is *never* exported outside of its process (unless of course a developer writes a specific method for that).

Encapsulation is ensured, as the instance is the only process able to access its own state. On method ending, the instance then just loops again, with an updated state.

One should therefore see a WOOPER instance as primarily a main loop which keeps the instance state:

 - it is waiting idle for any incoming (WOOPER) message
 - when such a message is received, based on the actual class of the instance and on the method name specified in the cal, the appropriate function defined in the appropriate module is selected by WOOPERl, taking into account the inheritance graph (actually a direct per-class mapping was already determined at start-up, for increased performances)
 - then this function is called with the appropriate parameters
 - if the method is a request, the specified result is sent back to the caller
 - then the instance loops again, on a state possibly updated by this method call

Thus the caller will only receive the **result** of a method, if it is a request. Otherwise, i.e. with oneways, nothing is sent back.

More precisely, depending on its returning a specific result, the method signature will correspond either to the one of a request or of a oneway, and will use in its body, respectively, either the ``wooper_return_state_result`` or the ``wooper_return_state_only`` macro to ensure that a state *and* a result are returned, or just a state.

A good practise is to add a comment to each method definition, and to specify whether it is a request or a oneway, if it is a const method, etc. For example, the previous method could be best written as::

  % Returns the current color of the whiskers of that cat instance.
  % (const request)
  getWhiskerColor(State) ->
	  ?wooper_return_state_result( State, ?getAttr(whisker_color) ).



.. Note:: When a constructor or a method determines that a fatal error should be raised (for example because it cannot find a required registered process), it should use ``throw``, like in: ``throw( {invalid_value,V} )``. Using ``exit`` is supported but not recommended.



For Requests
____________

Requests will use ``?wooper_return_state_result(NewState,Result)``: the new state will be kept by the instance, whereas the result will be sent to the caller. Hence ``wooper_return_state_result`` means that the method returns a state **and** a result.

For example::

 getAge(State) ->
 		  ?wooper_return_state_result(State,?getAttr(age)).


All methods are of course given the parameters specified at their call.

For example, we can declare::

 giveBirth(State,NumberOfMaleChildren,NumberOfFemaleChildren) ->
 		  [..]


And then we may call it, in the case of a cat having 2 male kitten and 3 female ones, with::

  MyCat ! {giveBirth,[2,3],self()}.


Requests can access to one more information than oneways: the PID of the caller that sent the request. As WOOPER takes care automatically of sending back the result to the caller, having the request know explicitly the caller is usually not useful, thus the caller PID does not appear explicitly in request signatures, among the actual parameters.

However WOOPER keeps track of this information, which remains available to methods.

The caller PID can indeed be retrieved from a request body by using the ``getSender`` macro, which is automatically managed by WOOPER::

  giveBirth(State,NumberOfMaleChildren,NumberOfFemaleChildren) ->
	CallerPID = ?getSender(),
	[..]


Thus a request has access to its caller PID without having to specify it twice, i.e. with no need to specify it in the parameters as well as in the third element of the call tuple: instead of
``MyCat ! {giveBirth,[2,3,self()],self()}.``, only ``MyCat ! {giveBirth,[2,3],self()}.`` can be used, while still letting the possibility for the called request (here ``giveBirth/3``, for a state and two parameters) to access the caller PID thanks to the ``getSender`` macro, and maybe store it for a later use or do anything appropriate with it.

Note that having to handle explicitly the caller PID is rather uncommon, as WOOPER takes care automatically of the sending of the result back to the caller.

The ``getSender`` macro should only be used for requests, as of course the sender PID has no meaning in the case of oneways.

If that macro is called nevertheless from a oneway, then it returns the atom ``undefined``.



For Oneways
___________

Oneway will use ``?wooper_return_state_only(NewState)``: the instance state will be updated, but no result will be returned to the caller, which is not even known.

For example::

  setAge(State,NewAge) ->
	?wooper_return_state_only( ?setAttribute(State,age,NewAge) ).

can be called that way::

  MyCat ! {setAge,4}.
  % No result to expect.


Oneways may leave the state unchanged, only being called for side-effects, for example::

  displayAge(State) ->
	io:format("My age is ~B~n.",[ ?getAttr(age) ]),
	?wooper_return_state_only(State).



Usefulness Of These Two Return Macros
_____________________________________

The two macros are actually quite simple, they are just here to structure the method implementations (helping the method developer not mixing updated states and results), and to help ensuring, in debug mode, that methods return well-formed results: an atom is then prepended to the returned tuple and WOOPER matches it during post-invocation, before handling the return, for an increased safety.

For example, in debug mode, ``?wooper_return_state_result(AState,AResult)`` will simply translate into ``{wooper_result,AState,AResult}``, and when the execution of the method is over, the WOOPER main loop of this instance will attempt to match the method returned value with that triplet (3-tuple).

Similarly, ``?wooper_return_state_only(AState)`` will translate into ``{wooper_result,AState}``.

If not in debug mode, then the ``wooper_result`` element will not be used in the returned tuples, for example ``?wooper_return_state_result(AState,AResult)`` will just be ``{AState,AResult}``.

Performances should increase a bit, at the expense of a less safe checking of the values returned by methods.

The two ``wooper_return_state_*`` macros have been introduced so that the unwary developer does not forget that his requests should not only return a result, by also a state, and that the order is always: first the state, then the result, not the other way round.



Self-Invocation: Calling a Method From The Instance Itself
..........................................................

When implementing a method of a class, one may want to call other methods **of that same class**, which are possibly overridden.

For example, when developing a ``declareBirthday`` method of ``class_Mammal`` (which among other things has to increment the mammal age), you may want to perform a call to the ``setAge`` method of the current instance.

If you just call ``setAge`` or ``class_Mammal:setAge``, then you will never call the potentially overloaded versions from child classes: if an instance of child class ``class_Cat`` (which inherited ``declareBirthday`` "as is") overloaded ``setAge``, you may want that ``declareBirthday`` calls automatically ``class_Cat:setAge`` instead of ``class_Mammal:setAge``.

Such a call can be easily performed asynchronously: a classical message-based method call can be used, like in ``self() ! {setAge,10}``. If this approach is useful when not directly needing from the method the result of the call and/or not needing to have it executed at once, there are cases when one wants to have that possibly overridden method being executed *directly* and to access to the corresponding updated state and, possibly, output result.

In these cases, one should call the WOOPER-defined ``executeRequest`` or ``executeOneway`` function, depending on the type of the method to call.

These two helper functions behave quite similarly to the actual method calls that are based on the operator ``!``, except that no target instance has to be specified (since it is a call made by an instance to itself) and that no message exchange is involved: the method look-up is just performed through the inheritance hierarchy, the correct method is called with the specified parameters and the result is then directly returned.

More precisely, ``executeRequest`` is ``executeRequest/3`` or ``executeRequest/2``, its parameters being the current state, the name of the request-method, and, if specified, the parameters of the called request, either as a list or as a standalone one.

``executeRequest`` returns a pair, made of the new state and of the result.

For example:

 - request taking more than one parameter, or one list parameter: ``{NewState,Result} = executeRequest(CurrentState, my_request_name, [ "hello", 42 ])``

 - request taking exactly one (non-list) parameter: ``{NewState,Result} = executeRequest(CurrentState, another_request_name, 42)``

 - request taking no parameter: ``{NewState,Result} = executeRequest(CurrentState, third_request_name)``




Regarding now ``executeOneway``, it is either ``executeOneway/3`` or ``executeOneway/2``, depending on whether the oneway takes parameters. If yes, they can be specified as a list (if there are more than one) or as a standalone parameter.

``executeOneway`` returns the new state.

For example:

 - oneway taking more than one parameter, or one list parameter: ``NewState = executeOneway(CurrentState,my_oneway_name,[ "hello", 42 ])``

 - oneway taking exactly one (non-list) parameter: ``NewState = executeOneway(CurrentState,another_oneway_name,42)``

 - oneway taking no parameter: ``NewState = executeOneway(CurrentState,third_oneway_name)``


.. Note:: As discussed previously, there are caller-side errors that are not expected to crash the instance. If such a call is performed directly from that instance (i.e. with one of the ``execute*`` constructs), then two errors will be output: the first, non-fatal for the instance, due to the method call, then the second, fatal for the instance, due to the failure of the ``execute*`` call. This is the expected behaviour, as here the instance plays both roles, the caller and the callee.


Finally, we can specify explicitly the class defining the version of the method that we want to execute, bypassing the inheritance-aware overriding system.

For example, a method needing to call ``setAge/2`` from its body would be expected to use something like: ``AgeState = executeOneway(State,setAge,NewAge)``.

If ``class_Cat`` overrode ``setAge/2``, any cat instance would then call the overridden ``class_Cat:setAge`` method instead of the original ``class_Creature:setAge``.

What if the method in our cat instance wanted, for any reason, to call the ``class_Creature`` version? In this case a ``execute*With`` function should be used.

These functions, which are ``executeRequestWith/3``, ``executeRequestWith/4``, ``executeOnewayWith/3`` and ``executeOnewayWith/4``, behave exactly as the previous ``execute*`` functions, except that they take an additional parameter (to be specified just after the state) which is the name of the mother class (direct or not) having defined the desired version of the method.

.. Note::

	This mother class does not have to have specifically defined or overridden that method: this method will just be called in the context of that class, as if it was an instance of the mother class rather than one of the actual child class.


In our example, we should thus use simply: ``AgeState = executeOnewayWith(State,class_Creature,setAge,NewAge)``, in order to call the ``class_Creature`` version of the ``setAge`` method.



Static Methods
..............

Static methods, as opposed to member methods, do not target specifically an instance, they are defined at the class level.

They thus do not operate on PID, they are just to be called thanks to their module name, exactly as any exported function.

Static methods are to be listed by the class developer thanks to the ``get_static_methods/0`` function, which must return a list whose elements are pairs, whose first part is the name (atom) of the static method, the second part being the arity of the static method.

For example::

  % Determines what are the static methods of this class (if any):
  get_static_methods() ->
	[ {get_default_whisker_color,0}, {compute_mew_frequency,2} ].


WOOPER exports automatically static methods, so that they can be readily called, as in::

  MyStaticMethods = class_Cat:get_default_whisker_color(),
  [..]


Hence static methods can be called from anywhere, no qualifier like public, protected or private apply to them.


:raw-latex:`\pagebreak`


.. _`state management`:

State Management
----------------

Principles
..........

We are discussing here about how an instance is to manage its inner state.

Its state is only directly accessible from inside the instance, from the body of its methods, whether they are inherited or not: the state of an instance is private (local to its process), and the outside can *only* access to it through the methods supported by the class of this instance.

An instance state (the one which is given by WOOPER as the ``State`` variable, first parameter of all methods) is defined as a **set of attributes**.

Each attribute is designated by a name, defined as an atom, and is associated to a mutable value, which can be any Erlang term.

The current state of an instance can be thought as a list of ``{attribute_name,attribute_value}`` pairs, like in: ``[ {color,black} , {fur_color,sand}, {age,5} , {name,"Tortilla"} ]``.




State Implementation Details
............................


Current Implementation
______________________

Starting from the 2.x versions of WOOPER, the list of attributes which defines a state is a class-specific, inheritance-aware, predetermined record.

This record gathers exactly all attributes of an instance: the ones that were defined directly in its class, as well as the ones that were inherited, directly or not.

This record is defined at compile-time, thanks to parse transforms. Once these mechanisms to determine it have been set-up, it is surely the solution that allows for the best overall performances.

So a class developer just has to specify the list of attributes that this class specifically introduces: all other attributes are inherited, and thus will be automatically deduced, at compile-time, from the list of the specified superclasses.

Class-specific attributes can be declared with some qualifiers.

More generally an attribute can be declared with:

 - just its name, ex: ``whisker_color``
 - a pair made of its name and a single qualifier, ex: ``{fur_color,protected}``
 - a pair made of its name and a list of qualifiers, ex: ``{mew_volume,[private,{const,35}]}``


Known attribute qualifiers are:

 - in terms of accessibility:

   - ``public``: for this attribute, a getter/setter pair is automatically generated; for example if ``whisker_color`` is declared as public, then ``getWhiskerColor/1`` and ``setWhiskerColor/2`` are automatically defined by WOOPER
   - ``protected``: the attribute can be modified either by the class that defined it or by any of its child classes
   - ``private``: the attribute can be modified only by the class that defined it, not by any of its child classes

 - in terms of mutability:

   - ``{const,Value}``: the value of the attribute will never change over time, none can modify it (once an attribute is const, there is no point in specifying that his access is protected or private)


Unless specified otherwise, an attribute is protected and non-const.


For example an attribute declaration can be::

  % Determines what are the class-specific attributes of this class (if any):
  get_attributes() ->
  	[ {fur_color,protected}, whisker_color, {mew_volume,[private,{const,35}]} ].


Once the instance will be created by WOOPER, the initial state will notably be made of a record, whose fields are exactly the attributes supported by this class, whether they are class-specific or inherited (directly or not).

Const attributes will already be set to their associated values, all others being initially set to the value ``undefined``.

This empty initial state will be given to the constructor, so that it is able first to call the counterpart constructors of the direct mother classes to update this state, then to set class-specific values afterwards, before returning the resulting state.


Previous Implementation
_______________________

The conceptual attribute list used to be a `hashtable <http://en.wikipedia.org/wiki/Hashtable>`_, selected for genericity, dynamicity and efficiency reasons (compared to other means of storing entries *a priori*, i.e. without prior knowledge about them).

The hash value of a key (like the ``age`` key) was computed, to be used as an index in order to find the corresponding value (in the previous example, ``5``) in the relevant bucket of the table.

The point was that this kind of look-up is performed in constant time on average, regardless of how many key/value pairs were stored in the table, whereas most dynamic data structures, like plain lists, would have look-up runtime costs that would increase with the number of pairs they contain, thus being possibly most often slower than their hashtable-based counterparts.

Using now class-specific fixed records has not real impact on flexibility, and allows for constant-time operations significantly more effective than a hashtable, being both faster, and smaller in memory.




:raw-latex:`\pagebreak`


Managing The State Of An Instance
.................................

A set of functions allows to operate on these state variables, notably to read and write the attributes they contain.

As seen in the various examples, method implementations will access (read/write) to attributes stored in the instance state, whose original version (i.e. the state of the instance at the method beginning) is always specified as their first parameter, named ``State``.

This current state can be then modified in the method, and a new state (usually an updated version of the initial one) will be returned locally to WOOPER, thanks to the final statement in the method, one of the two ``wooper_return_state_*`` macros.

Then the code automatically instantiated by the WOOPER header in the class implementation will loop again with the updated state for this instance, waiting for the next method call, that will possibly change it and trigger side-effects, and so on.

See `wooper.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper.hrl?view=markup>`_ for the actual definition of most of these WOOPER constructs.

These state-management constructs look like functions but, thanks to parse transforms, they are actually inlined for increased performances.

As a consequence of the change in the underlying data structure for state variables, following state-management functions have been deprecated for the 2.x versions of WOOPER and onward: ``removeAttribute/2``, ``hasAttribute/2``.




Modifying State
_______________


The ``setAttribute/3`` Function
*******************************

Setting an attribute (creating and/or modifying it) should be done with the **setAttribute** function: ``NewState = setAttribute(AState,AttributeName,NewAttributeValue)``.

For example, ``AgeState = setAttribute(State,age,3)`` will return a new state, bound to ``AgeState``, exact copy of ``State`` (with all the attribute pairs equal) but for the ``age`` attribute, whose value will be set to 3.

.. comment (whether or not this attribute was already defined in ``State``).

Therefore, during the method execution, any number of states can be defined (ex: ``State``, ``InitialisedState``, ``AgeState``, etc.), before all, but the one that is returned, are garbage-collected.

Note that the corresponding state duplication remains efficient both in terms of processing and memory, as the different underlying state structures (ex: ``State`` and ``AgeState``) actually **share** all their terms except the one modified, thanks to the immutability of Erlang variables, which allows to reference rather than copy, be these datastructures hashtables, records, or anything else.

In various cases, notably in constructors, one needs to define a series of attributes in a row, but chaining ``setAttribute`` calls with intermediate states is not really convenient.

A better solution is to use the **setAttributes** function (note the plural) to set a list of attribute name/attribute value pairs in a row.

For example, ``ConstructedState = setAttributes(State,[ {age,3}, {whisker_color,white} ])`` will return a new state, exact copy of ``State`` but for the listed attributes, set to their respective specified value.




The ``removeAttribute/2`` Function
**********************************


.. Note::

 The **removeAttribute** function is now deprecated and should not be used any more.


This fonction was used in order to fully remove an attribute entry (i.e. the whole key/value pair).

This function is deprecated now, as we prefer defining all attributes once for all, at construction time, and never add or remove them dynamically: the good practise is just to operate on their value, which can by example be set to ``undefined``, without having to deal with the fact that, depending on the context, a given attribute may or may not be defined.

For example ``NewState = removeAttribute(State,an_attribute)`` could be used, for a resulting state having no key corresponding to ``an_attribute``.


Neither ``setAttribute`` nor ``removeAttribute`` can fail, regardless of the attribute being already existing or not.




Reading State
_____________


The ``hasAttribute/2`` Function
*******************************

.. Note::

 The **hasAttribute** function is now deprecated and should not be used any more.


To test whether an attribute is defined, use the **hasAttribute** function: ``hasAttribute(AState,AttributeName)``, which returns either ``true`` or ``false``, and cannot fail.

For example, ``true = hasAttribute(State,whisker_color)`` matches if and only if the attribute ``whisker_color`` is defined in state ``State``.

Note that generally it is a bad practice to define attributes outside of the constructor of an instance, as the availability of an attribute could then depend on the actual state, which is an eventuality generally difficult to manage reliably.

A better approach is instead to define all possible attributes directly from the constructor. They would then be assigned to their initial value and, if none is appropriate, they should be set to the atom ``undefined`` (instead of not being defined at all).



The ``getAttribute/2`` Function
*******************************

Getting the value of an attribute should be done with the **getAttribute** function: ``AttributeValue = getAttribute(AState,AttributeName)``.

For example, ``MyColor = getAttribute(State,whisker_color)`` returns the value of the attribute ``whisker_color`` from state ``State``.

The requested attribute may not exist in the specified state. In this case, a compile-time error is issued.

With the hashtable-based version of WOOPER, requesting a non-existing attribute triggered a bad match. In the previous example, should the attribute ``whisker_color`` not have been defined, ``getAttribute`` would return: ``{{badmatch,{hashtable_key_not_found,whisker_color}},[{hashtable,getEntry,2},..``.



The ``getAttr/2`` Macro
***********************

Quite often, when having to retrieve the value of an attribute from a state variable, that variable will be named ``State``, notably when using directly the original state specified in the method declaration.

Indeed, when a method needs a specific value, generally either this value was already available in the initial state (then we can read it in ``State``), or is computed in the course of the method, in which case that value is often bound to a variable which can then be re-used directly.

In this case, the **getAttr** macro can be used: ``?getAttr(whisker_color)`` expands as ``getAttribute(State,whisker_color)``, and is a bit shorter.

This is implemented as a macro so that the user remains aware that an implicit variable named ``State`` is then used.

The less usual cases where a value must be read from a state variable which is *not* the initial ``State`` one occur mostly when wanting to read a value from the updated state returned by a ``execute*`` function call. In this case ``getAttribute/2`` should be used.





Read-Modify-Write Operations
____________________________

Some more helper functions are provided for the most common operations, to keep the syntax as lightweight as possible.



The ``addToAttribute/3`` Function
*********************************

The corresponding signature is ``NewState = addToAttribute(State,AttributeName,Value)``: when having a numerical attribute, adds specified number to the attribute.

For example: ``MyState = addToAttribute(FirstState,a_numerical_attribute,6)``. In ``MyState``, the value of attribute ``a_numerical_attribute`` is increased of 6, compared to the one in ``FirstState``.

Calling ``addToAttribute/3`` on a non-existing attribute will trigger a compile-time error. If the attribute exists, but no addition can be performed on it (meaningless for the type of the current value), a run-time error will be issued.


With the hashtable-based version of WOOPER:

 - if the target attribute does not exist, will trigger ``{{badmatch,undefined},[{hashtable,addToEntry,3},..``

 - if it exists but no addition can be performed on it (meaningless for the type of the current value), will trigger ``{badarith,[{hashtable,addToEntry,3},..``.



The ``subtractFromAttribute/3`` Function
****************************************

The corresponding signature is ``NewState = subtractFromAttribute(State,AttributeName,Value)``: when having a numerical attribute, subtracts specified number from the attribute.

For example: ``MyState = subtractFromAttribute(FirstState,a_numerical_attribute,7)``. In ``MyState``, the value of attribute ``a_numerical_attribute`` is decreased of 7, compared to the one in ``FirstState``.


Calling ``subtractFromAttribute/3`` on a non-existing attribute will trigger a compile-time error. If the attribute exists, but no subtraction can be performed on it (meaningless for the type of the current value), a run-time error will be issued.


With the hashtable-based version of WOOPER:

 - if the target attribute does not exist, will trigger ``{{badmatch,undefined},[{hashtable,subtractFromEntry,3},..``

 - if it exists but no addition can be performed on it (meaningless for the type of the current value), will trigger ``{badarith,[{hashtable,subtractFromEntry,3},..``.




The ``toggleAttribute/2`` Function
**********************************

The corresponding signature is ``NewState = toggleAttribute(State,BooleanAttributeName)``: when having a boolean attribute, whose value is either ``true`` or ``false``, sets the opposite logical value to the current one.

For example: ``NewState = ?toggleAttribute(State,a_boolean_attribute)``.

Calling ``toggleAttribute/2`` on a non-existing attribute will trigger a compile-time error. If the attribute exists, but has not a boolean value, a run-time error will be issued.


With the hashtable-based version of WOOPER:

 - if the target attribute does not exist, will trigger ``{{case_clause,undefined},[{hashtable,toggleEntry,2},..``.

 - if it exists but is neither true or false, will trigger ``{{case_clause,{value,..}},[{hashtable,toggleEntry,2},..``.



The ``appendToAttribute/3`` Function
************************************

The corresponding signature is ``NewState = appendToAttribute(State,AttributeName,Element)``: when having a list attribute, appends specified element to the attribute list, in first position.

For example, if ``a_list_attribute`` was already set to ``[see_you,goodbye]`` in ``State``, then after ``NewState = appendToAttribute(State,a_list_attribute,hello)``, the ``a_list_attribute`` attribute defined in ``NewState`` will be equal to ``[hello,see_you,goodbye]``.

Calling ``appendToAttribute/3`` on a non-existing attribute will trigger a compile-time error. If the attribute exists, but is not a list, an ill-formed list will be created (ex: ``[8|false]`` when appending 8 to ``false``, which is not a list).

With the hashtable-based version of WOOPER:

 - if the target attribute does not exist, will trigger ``{{badmatch,undefined},[{hashtable,appendToEntry,3},..``.

 - if it exists but is not already a list, it will not crash but will create an ill-formed list (ex: ``[8|false]`` when appending 8 to ``false``, which is not a list).



The ``deleteFromAttribute/3`` Function
**************************************

The corresponding signature is ``NewState = deleteFromAttribute(State,AttributeName,Element)``: when having a list attribute, deletes first match of specified element from the attribute list.

For example: ``NewState = deleteFromAttribute(State,a_list_attribute,hello)``, with the value corresponding to the ``a_list_attribute`` attribute in ``State`` variable being ``[goodbye,hello,cheers,hello,see_you]`` should return a state whose ``a_list_attribute`` attribute would be equal to ``[goodbye,cheers,hello,see_you]``, all other attributes being unchanged.

If no element in the list matches the specified one, no error will be triggered and the list will be kept as is.


Calling ``deleteFromAttribute/3`` on a non-existing attribute will trigger a compile-time error. If the attribute exists, but is not a list, a run-time error will be issued.

With the hashtable-based version of WOOPER:

 - if the target attribute does not exist, will trigger ``{{badmatch,undefined},[{hashtable,deleteFromEntry,3},..``.

 - if it exists but is not already a list, it will trigger ``{function_clause,[{lists,delete,[..,..]},{hashtable,deleteFromEntry,3}``.




The ``popFromAttribute/2`` Function
***********************************

The corresponding signature is ``{NewState,Head} = popFromAttribute(State,AttributeName)``: when having an attribute of type list, this function removes the head from the list and returns a pair made of the updated state (same state except that the corresponding list attribute has lost its head, it is equal to the list tail now) and of that head.

For example: ``{NewState,Head} = popFromAttribute(State,a_list_attribute)``. If the value of the attribute ``a_list_attribute`` was ``[5,8,3]``, its new value (in ``NewState``) will be ``[8,3]`` and ``Head`` will be bound to ``5``.



The ``addKeyValueToAttribute/4`` Function
*****************************************

The corresponding signature is ``NewState = addKeyValueToAttribute(State,AttributeName,Key,Value)``: when having an attribute whose value is a hashtable [#], adds specified key/value pair to that hashtable attribute.

.. [#] Therefore, with the hashtable-based version of WOOPER, it is a hashtable in the WOOPER hashtable.


For example: ``TableState = setAttribute( State, my_hashtable, hashtable:new() ), NewState = addKeyValueToAttribute( TableState, my_hashtable, my_key, my_value )`` will result in having the attribute ``my_hashtable`` in state variable ``TableState`` being an hashtable with only one entry, whose key is ``my_key`` and whose value is ``my_value``.





:raw-latex:`\pagebreak`


Multiple Inheritance & Polymorphism
-----------------------------------


The General Case
................

Both multiple inheritance and polymorphism are automatically managed by WOOPER: even if our cat class does not define a ``getAge`` method, it can nevertheless readily be called on a cat instance, as it is inherited from its mother classes (here from ``class_Creature``, an indirect mother class).

Therefore all creature instances can be handled the same, regardless of their actual classes::

  % Inherited methods work exactly the same as methods defined
  % directly in the class:
  MyCat ! {getAge,[],self()},
  receive
 		   {wooper_result,Age} ->
 		 		   io:format( "This is a ~B year old cat.", [Age] )
  end,

  % Polymorphism is immediate:
  % (class_Platypus inheriting too from class_Mammal,
  % hence from class_Creature).
  MyPetList = [ MyCat, MyPlatypus ],
  foreach(
 		   fun(AnyCreature) ->
 		 		   AnyCreature ! {getAge,[],self()},
 		 		   receive
 		 		 		   {wooper_result,Age} ->
 		 		 		 		   io:format( "This is a ~B year old creature.", [Age] )
 		 		   end,
 		   MyPetList).

should output something like::

 This is a 4 year old creature.
 This is a 9 year old creature.


The point here is that the implementer does not have to know what are the actual classes of the instances he handles, provided they share a common ancestor: polymorphism allows to handle them transparently.


The Special Case of Diamond-Shaped Inheritance
..............................................

In the case of a `diamond-shaped inheritance <http://en.wikipedia.org/wiki/Diamond_problem>`_, as the method table is constructed in the order specified in the declaration of the superclasses (``get_superclasses() -> [class_X,class_Y, etc.]).``), and as child classes override mother ones, when an incoming WOOPER message arrives the selected **method** should be the one defined in the last inheritance branch of the last child (if any), otherwise the one defined in the next to last branch of the last child, etc.

Generally speaking, overriding in that case the relevant methods that were initially defined in the child class at the base of the diamond so that they perform explicitly a direct call to the wanted module is by far the most reasonable solution, in terms of clarity and maintainability, compared to guessing which version of the method will be called in the inheritance graph.

Regarding the instance state, the **attributes** are set by the constructors, therefore the developer can select in which order the direct mother classes should be constructed. However it always leads to calling multiple times the constructor of the class that sits at the top of the diamond. Any side-effect it would induce would then occur as many times as this class is a common ancestor of the actual class.

.. Note:: More generally speaking, diamond-shaped inheritance is seldom necessary. More often than not, it is the consequence of a bad OOP design, and should be avoided anyway.





:raw-latex:`\pagebreak`

Life-Cycle
----------

Basically, creation and destruction of instances are managed respectively thanks to the ``new``/``new_link`` and the ``delete`` operators (all these operators are WOOPER-reserved function names, for all arities)::

  MyCat = class_Cat:new(Age,Gender,FurColor,WhiskerColor),
  MyCat ! delete.




Instance Creation: ``new``/``new_link`` And ``construct``
.........................................................


Role of a new/construct Pair
____________________________

Whereas the purpose of ``new``/``new_link`` is to create a working instance on the user's behalf, the role of ``construct`` is to initialise an instance of that class while being able to be chained for inheritance, as explained later.

All calls to a ``new`` operator result in an underlying call to the corresponding ``construct`` operator.

For example, both ``MyCat = class_Cat:new(A,B,C,D)`` and ``MyCat = class_Cat:new_link(A,B,C,D)``will rely on ``class_Cat:construct/5`` to set-up a proper initial state for the ``MyCat`` instance: ``class_Cat:construct(State,A,B,C,D)`` will be called in both cases.

The ``new_link`` operator behaves exactly as the ``new`` operator, except that it creates an instance that is Erlang-linked with the process that called that operator, exactly like ``spawn_link`` behaves compared to ``spawn``.

The ``new`` and ``new_link`` operators are automatically defined by WOOPER, but they rely on the class-specific user-defined ``construct`` operator (only WOOPER is expected to call this operator). This ``construct`` method is the one that must be implemented by the class developer.

Multiple ``construct`` operators can be defined, which are selected based on arity and pattern-matching.

For example::

  % Selection based on arity:
  MyFirstDog  = Class_Dog:new( "Skippy"),
  MySecondDog = Class_Dog:new( create_from_age, 5 ),

  % Selection based on pattern-matching:
  MyThirdDog  = Class_Dog:new( create_from_weight, 4.4 ).
  MyFourthDog = Class_Dog:new( create_from_colors, [sand,white] ), .




The Various Ways of Creating An Instance
________________________________________

As shown with the ``new_link`` operator, even with the same set of constructing parameters, many variations of ``new`` can be imagined: linked or not, synchronous or not, with a time-out or not, on current node or on a user-specified one, etc.

For a class whose instances can be constructed from N actual parameters, the following construction operators, detailed in the next section, are built-in:

  - if instance is to be created on the **local** node:

	- non-blocking creation: ``new/N`` and ``new_link/N``
	- blocking creation: ``synchronous_new/N`` and ``synchronous_new_link/N``
	- blocking creation with time-out: ``synchronous_timed_new/N`` and ``synchronous_timed_new_link/N``

  - if instance is to be created on any specified **remote** node:

	- non-blocking creation: ``remote_new/N+1`` and ``remote_new_link/N+1``
	- blocking creation: ``remote_synchronous_new/N+1`` and ``remote_synchronous_new_link/N+1``
	- blocking creation with time-out: ``remote_synchronous_timed_new/N+1`` and ``remote_synchronous_timed_new_link/N+1``

.. Note:: All ``remote_*`` variations require one more parameter (to be specified first), since the remote node on which the instance should be created has of course to be specified.


All supported ``new`` variations are detailed below.


Asynchronous new
****************

This corresponds to the plain ``new``, ``new_link`` operators etc. discussed earlier. These basic operators are **asynchronous** (non-blocking): they trigger the creation of a new instance, and return immediately, without waiting for it to complete, and the execution of the calling process continues while the instance is being created and executed.


Synchronous new
***************

With the previous asynchronous forms, the caller has no way of knowing when the spawned instance is up and running (if it ever happens).

Thus two counterpart operators, ``synchronous_new/synchronous_new_link`` are also available.

They behave like ``new/new_link`` except they will return only when (and if) the created instance is up and running: they are blocking, synchronous, operators.

For example, after ``MyMammal = class_Mammal:synchronous_new(...)``, one knows that the ``MyMammal`` instance is fully created and waiting for incoming messages.

The implementation of these synchronous operations relies on a message (``{spawn_successful,InstancePid}``) being automatically sent by the created instance to the WOOPER code on the caller side, so that the ``synchronous_new`` operator will return to the user code only once successfully constructed and ready to handle messages.


Timed Synchronous new
*********************

Note that, should the instance creation fail, the caller of a synchronous new would then be blocked for ever, as the awaited message would actually never be sent.

This is why the ``synchronous_timed_new*`` operators are defined: if the time-out (its default duration is 5 seconds) expires while waiting for the created instance to answer, then they will throw an appropriate exception, among:

 - ``{synchronous_time_out,InstanceModule}``
 - ``{synchronous_linked_time_out,InstanceModule}``
 - ``{remote_synchronous_time_out,Node,InstanceModule}``
 - ``{remote_synchronous_linked_time_out,Node,InstanceModule}``
 - ``{synchronous_time_out,InstanceModule}``
 - ``{synchronous_linked_time_out,InstanceModule}``
 - ``{remote_synchronous_time_out,Node,InstanceModule}``
 - ``{remote_synchronous_linked_time_out,Node,InstanceModule}``

Then the caller may or may not catch this exception.


.. comment return the ``time_out`` atom instead of the PID of the created instance. The caller is then able to check whether the creation succeeded thanks to a simple pattern-matching.


Remote new
**********

Exactly like a process might be spawned on another Erlang node, a WOOPER instance can be created on any user-specified available Erlang node.

To do so, the ``remote_*new*`` variations shall be used. They behave exactly like their local counterparts, except that they take an additional information, as first parameter: the node on which they must be created.

For example: ``MyCat = class_Cat:remote_new(TargetNode,Age,Gender,FurColor,WhiskerColor ).``

Of course:

 - the remote node must be already existing
 - the current node must be able to connect to it (shared cookie)
 - all modules that the instance will make use of must be available on the remote node, including the ones of all relevant classes (i.e. the class of the instance but also its mother classes)

All variations of the ``new`` operator are always defined automatically by WOOPER: nothing special is to be done for them, provided of course that the corresponding constructor is defined.


Some Examples Of Instance Creation
__________________________________

Knowing that a cat can be created out of four parameters (Age, Gender, FurColor, WhiskerColor), various cat instances could be created thanks to::

  % Local asynchronous creation:
  MyFirstCat = class_Cat:new( 1, male, brown, white ),

  % The same, but a crash of this cat will crash the current process too:
  MySecondCat = class_Cat:new_link( 2, female, black, white ),

  % This cat will be created on OtherNode, and the call will return only once
  % it is up and running or once the creation failed. As moreover the cat
  % instance is linked to the instance process, it may crash this calling
  % process:
  MyThirdCat = class_Cat:remote_synchronous_timed_new_link( OtherNode, 3,
	male, grey, black ),

  case MyThirdCat of

  	CatPid when is_pid(CatPid) ->
	  MyThirdCat ! declareBirthday;

	time_out ->
	  [..]

  end,
  [..]





Definition of the ``construct`` Operators
_________________________________________

Each class must define at least one ``construct`` operator, whose role is to fully initialise the state of new instances, based on specified construction parameters, on compliance with the class inheritance, and regardless of the ``new`` variation being used.

In the context of class inheritance, the ``construct`` operators are expected to be chained: they must be designed to be called by the ones of their child classes, and they must call themselves the constructors of their direct mother classes, if any.

Hence they always take the current state of the instance being created as a starting base, and returns it once updated, first from the direct mother classes, then by this class itself.

For example, let's suppose ``class_Cat`` inherits directly from ``class_Mammal`` and from ``class_ViviparousBeing``, has only one attribute (``whisker_color)`` of its own, and that a new cat is to be created out of three pieces of information::

  [..]
  get_superclasses() ->
	[class_Mammal,class_ViviparousBeing].

  [..]
  get_attributes() ->
  	[ whisker_color ].

  % Constructs a new Cat.
  construct(State, Gender, FurColor, WhiskerColor) ->
	  % First the direct mother classes:
	  MammalState = class_Mammal:construct( State, _Age=0, Gender, FurColor ),
	  ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),
	  % Then the class-specific attributes:
	  setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).

The fact that the ``Mammal`` class itself inherits from the ``Creature`` class must not appear here: it is to be managed directly by ``class_Mammal:construct`` (at any given inheritance level, only direct classes must be taken into account).

One should ensure that, in constructors, the successive states are always built from the last updated one, unlike::

  % WRONG, the age update is lost:
  construct(State,Age,Gender) ->
	AgeState = setAttribute(State,age,Age),
	% AgeState should be used here, not State:
	setAttribute(State,gender,Gender),


This would be correct::

  % RIGHT but a bit clumsy:
  construct(State,Age,Gender) ->
	AgeState = setAttribute(State,age,Age),
	setAttribute(AgeState,gender,Gender).


Recommended form::

  % BEST:
  construct(State,Age,Gender) ->
	setAttributes( State, [ {age,Age}, {gender,Gender} ]).



.. Note::

  There is no strict relationship between construction parameters and instance attributes, neither in terms of cardinality, type or value.
  For examples, attributes could be set to default values, a point could be created from an angle and a distance but its actual state could consist ontwo cartesian coordinates instead, etc.
  Therefore both have to be defined by the class developer, and, in the general case, attributes cannot be inferred from construction parameters.


Finally, a class can define multiple constructors: the proper one will be called, based on its arity (determined thanks to the number of parameters specified) and on pattern-matching performed on these parameters, to select the relevant clause of the constructor.



Instance Deletion
.................


Automatic Chaining Of Destructors
_________________________________

We saw that, when implementing a constructor (``construct/N``), like in all other OOP approaches the constructors of the direct mother classes have to be explicitly called, so that they can be given the proper parameters, as determined by the class developer.

Conversely, with WOOPER, when defining a destructor for a class (``delete/1``), one only has to specify what are the *specific* operations and state changes (if any) that are required so that an instance of that class is deleted: the proper calling of the destructors of mother classes across the inheritance graph is automatically taken in charge by WOOPER.

Once the user-specified actions have been processed by the destructor (ex: releasing a resource, unsubscribing from a registry, deleting other instances, closing properly a file, etc.), it is expected to return an updated state, which will be given to the destructors of the instance superclasses.

WOOPER will automatically export and make use of any user-defined destructor.


Asynchronous Destructor: ``delete/1``
_____________________________________

More precisely, either the class implementer does not define at all a ``delete/1`` operator (and therefore uses the default do-nothing destructor), or it defines it explicitly, like in::

  delete(State) ->
	io:format("An instance of class ~w is being deleted now!", [?MODULE] ),
	% Quite often the destructor does not need to modify the instance state:
	State.


In both cases (default or user-defined destructor), when the instance will be deleted (ex: ``MyInstance ! delete`` is issued), WOOPER will take care of:

 - calling any destructor defined for that class
 - then calling the ones of the direct mother classes, which will in turn call the ones of their mother classes, and so on

Note that the destructors for direct mother classes will be called in the reverse order of the one according to the constructors ought to have been called: if a class ``class_X`` declares ``class_A`` and ``class_B`` as mother classes (in that order), then in the ``class_X:construct`` definition the implementer is expected to call ``class_A:construct`` and then ``class_B:construct``, whereas on deletion the WOOPER-enforced order of execution will be: ``class_X:delete``, then ``class_B:delete``, then ``class_A:delete``, for the sake of symmetry.


Synchronous Destructor: ``synchronous_delete/1``
________________________________________________

WOOPER automatically defines a way of deleting *synchronously* a given instance: a caller can request a synchronous (blocking) deletion of that instance so that, once notified of the deletion, it knows for sure the instance does not exist any more, like in::

  InstanceToDelete ! {synchronous_delete,self()},
  % Then the caller can block as long as the deletion did not occur:
  receive
  	{deleted,InstanceToDelete} ->
		doSomething()
  end.


The class implementer does not have to do anything to support this feature, as the synchronous deletion is automatically built by WOOPER on top of the usual asynchronous one (``delete/1``).




:raw-latex:`\pagebreak`


Miscellaneous Technical Points
==============================


``delete_any_instance_referenced_in/2``
---------------------------------------

When an attribute contains either a single instance reference (i.e. the PID of the corresponding process) or a list of instance references, this WOOPER-defined helper function will automatically delete (asynchronously) these instances, and will return an updated state in which this attribute is set to ``undefined``.

This function is especially useful in destructors.

For example, if ``State`` contains:

 - an attribute named ``my_pid`` whose value is the PID of an instance
 - and also an attribute named ``my_list_of_pid`` containing a list of PID instances

and if the deleted instance took ownership of these instances, then::

  delete(State) ->
	TempState = delete_any_instance_referenced_in( State, my_pid ),
	delete_any_instance_referenced_in( TempState, my_list_of_pid).

will automatically delete all these instances and return an updated state.

Then the destructors of the mother classes can be chained by WOOPER.



EXIT Messages
-------------

A class instance may receive EXIT messages from other processes.

A given class can process these EXIT notifications:

 - either by defining and exporting the ``onWooperExitReceived/3`` oneway
 - or by inheriting it

For example::

  onWooperExitReceived(State,Pid,ExitType) ->
	io:format( "MyClass EXIT handler ignored signal '~w'"
	  " from ~w.~n", [ExitType,Pid] ),
	?wooper_return_state_only(State).

may result in::

 ``MyClass EXIT handler ignored signal 'normal' from <0.40.0>.``


If no class-specific EXIT handler is available, the default WOOPER one will be used.

It will just notify the signal to the user, by displaying a message like::

 ``WOOPER default EXIT handler for instance <0.36.0> of class class_Cat ignored signal 'normal' from <0.40.0>.``



Practical Build Hints
---------------------

All WOOPER classes must include `wooper.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper.hrl?view=markup>`_: ``-include("wooper.hrl").``.

To help declaring the right defines in the right order, using the WOOPER `template <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Template.erl.sample?view=markup>`_ is recommended.

.. Note::

 To be updated, notably with respect to parse transforms.


One should have ``utils.beam``, ``hashtable.beam`` and ``wooper_class_manager.beam`` available to the interpreter before using WOOPER-based classes.


On UNIX-like platforms, using the Makefiles included in the WOOPER archive is recommended.

One just has to go at the root of the sources (from an extracted archive, you are expected to be in the ``wooper-x.y`` root directory) and simply run: ``make`` (assuming GNU make is available, so that the WOOPER ``GNUmakefile`` is used).

On other platforms, these modules must be compiled one way or another before using WOOPER. For example::

  1> c(wooper_class_manager).
  {ok,wooper_class_manager}


We provide as well a WOOPER-aware `neditrc <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/common/conf/nedit.rc>`_ configuration file for syntax highlighting (on black backgrounds), inspired from Daniel Solaz's `Erlang Nedit mode <http://www.trapexit.org/forum/viewtopic.php?p=30189>`_.


Similarity With Other Languages
-------------------------------

Finally, WOOPER is in some ways adding features quite similar to the ones available with other languages, including Python (simple multiple inheritance, implied ``self/State`` parameter, attribute dictionaries, etc.; with less syntactic sugar available though) while still offering the major strengths of Erlang (concurrency, distribution, functional paradigm) and not hurting too much the overall performances (mainly thanks to the prebuilt attribute and method tables).

Although the hashtable-based version of WOOPER is as permissive as Python, allowing to define dynamically new attributes at any time (i.e. outside of the "constructor"), the newer WOOPER versions enforce a stricter attribute management, closer to the one of languages like C++ or Java.



:raw-latex:`\pagebreak`


WOOPER Example
==============

We created a small set of classes allowing to show multiple inheritance:

.. figure:: wooper-example.png
   :alt: WOOPER Example
   :scale: 40

   Example of an inheritance graph to be handled by WOOPER


Class implementations
---------------------

 - `class_Creature.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Creature.erl?view=markup>`_

 - `class_ViviparousBeing.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_ViviparousBeing.erl?view=markup>`_

 - `class_OvoviviparousBeing.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_OvoviviparousBeing.erl?view=markup>`_

 - `class_Mammal.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Mammal.erl?view=markup>`_

 - `class_Reptile.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Reptile.erl?view=markup>`_

 - `class_Cat.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Cat.erl?view=markup>`_

 - `class_Platypus.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Platypus.erl?view=markup>`_




Tests
-----

 - `class_Creature_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Creature_test.erl?view=markup>`_

 - `class_ViviparousBeing_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_ViviparousBeing_test.erl?view=markup>`_

 - `class_OvoviviparousBeing_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_OvoviviparousBeing_test.erl?view=markup>`_

 - `class_Mammal_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Mammal_test.erl?view=markup>`_

 - `class_Reptile_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Reptile_test.erl?view=markup>`_

 - `class_Cat_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Cat_test.erl?view=markup>`_

 - `class_Platypus_test.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/examples/class_Platypus_test.erl?view=markup>`_


To run a test (ex: ``class_Cat_test.erl``), when everything is compiled one just has to enter: ``make class_Cat_run``.


:raw-latex:`\pagebreak`



Good Practises
==============

When using WOOPER, the following conventions are deemed useful to respect.

No warning should be tolerated in code using WOOPER, as we never found useless notifications.

With the hashtable-based version of WOOPER, all attributes of an instance should better be defined from the constructor, instead of being dynamically added during the life of the instance; otherwise the methods would have to deal with some attributes which may or may not be defined; if no proper value exists for an attribute at the creation of an instance, then its value should just be set to the atom ``undefined``.

When a function or a method is defined in a WOOPER file, it should of course be commented, and, even if the information can be guessed from context and body, in the last line of the comments the type of the function should be specified (ex: ``oneway``, ``request``, ``helper function``, etc.) possibly with qualifiers (ex: ``const``), like in::

  % Sets the current color.
  % (oneway)
  setColor(State,NewColor) ->
  	[..]

Helper functions and static methods (which, from an Erlang point of view, are just exported functions) should be named like C functions (ex: ``compute_sum``) rather than being written in CamelCase (ex: no helper function should be named ``computeSum``), to avoid mixing up these different kinds of code.

To further separate helper functions from instance methods, an helper function taking a ``State`` parameter should better place it at the end of its parameter list rather than in first position (ex: ``compute_sum(X,Y,State)`` rather than ``compute_sum(State,X,Y)``).

In a method body, the various state variables being introduced should be properly named, i.e. their name should start with a self-documenting prefix followed by the ``State`` suffix, like in: ``SeededState = setAttribute(State,seed,{1,7,11})``.



Some more general (mostly unrelated) Erlang conventions that we like:

 - when more than one parameter is specified in a fonction signature, parameter names can be surrounded by spaces (ex: ``f(Color)``, whereas ``g( Age, Height )``)

 - functions should be separated by (at least) three newlines, whereas clauses for a given function should be separated exactly by one newline

 - to auto-document parameters, a "mute" variable is preferably to be used: for example, instead of ``f(Color,true)`` use ``f( Color, _Dither = true )``; however note that these mute variables are still bound and pattern-matched: for example, if multiple ``_Dither`` mute variables are bound in the same scope to different values, a bad match will be triggered






:raw-latex:`\pagebreak`

Troubleshooting
===============


General Case
------------


Compilation Warnings
....................

A basic rule of thumb in all languages is to enable all warnings and eradicate them before even trying to test a program.

This is still more valid when using WOOPER, whose proper use should never result in any warning being issued by the compiler.

Notably warnings about unused variables allow to catch mistakes when state variables are not being properly taken care of (ex: when a state is defined but never re-used later).



Runtime Errors
..............

Most errors while using WOOPER should result in relatively clear messages (ex: ``wooper_method_failed`` or ``wooper_method_faulty_return``), associated with all relevant run-time information that was available to WOOPER.

Another way of overcoming WOOPER issues is to activate the debug mode for all WOOPER-enabled compiled modules (ex: uncomment ``-define(wooper_debug,).`` in ``wooper.hrl``), and recompile your classes.

The debug mode tries to perform extensive checking on all WOOPER entry points, from incoming messages to the user class itself, catching mistakes from the class developer as well as from the class user.

For example, the validity of states returned by a constructor, by each method and by the destructor is checked, as the one of states specified to the ``execute*`` constructs.

If it is not enough to clear things up, an additional step can be to add, on a per-class basis (ex: in ``class_Cat.erl``), before the WOOPER include, ``-define(wooper_log_wanted,).``.

Then all incoming method calls will be traced, for easier debugging. It is seldom necessary to go till this level of detail.

As there are a few common WOOPER gotchas though, the main ones are listed below.


Mismatches In Method Call
_________________________


Oneway Versus Request Calls
***************************

One of these gotchas - experienced even by the WOOPER author - is to define a two-parameter oneway, whose second parameter is a PID, and to call this method wrongly as a request, instead of as a oneway.

For example, let's suppose the ``class_Dog`` class defines the oneway method ``startBarkingAt/3`` as::

  startBarkingAt(State,Duration,ListenerPID) -> ...


The correct approach to call this **oneway** would be::

  MyDogPid ! {startBarkingAt,[MyDuration,self()]}


An absent-minded developer could have written instead::

  MyDogPid ! {startBarkingAt, MyDuration, self()}


This would have called a ``request`` method ``startBarkingAt/2`` (which could have been for example ``startBarkingAt(State,TerminationOffset) -> ...``, the PID being interpreted by WOOPER as the request sender PID), method which most probably would not exist.

This would result in a bit obscure error message like ``Error in process <0.43.0> on node 'XXXX' with exit value: {badarg,[{class_Dog,wooper_main_loop,1}]}``.


List Parameter Incorrectly Specified In Call
********************************************

As explained in the `single method parameter is a list`_ section, if a method takes only one parameter and if this parameter is a list, then in a call this parameter cannot be specified as a standalone one: a parameter list with only one element, this parameter, should be used instead.



Error With Exit Value: ``{undef,[{hashtable,new,[..]}..``
_________________________________________________________

You most probably are using the hashtable-based version of WOOPER and you forgot to build the ``common`` directory, which, among other things, contains the ``hashtable.erl`` source file.

Check that you have a ``hashtable.beam`` file indeed, and that it can be found from the paths specified to the interpreter.




:raw-latex:`\pagebreak`


For code generated by WOOPER version 2.0 and more recent
--------------------------------------------------------

For example for debugging or tuning purposes, one may want to access to the Erlang code generated specifically for a given class by the WOOPER parse transform.

To do so, one should just make use, in the source file of the class (ex: ``class_Cat.erl``), after the module declaration, of the ``pt_pp_src`` (for ``parse_trans-pretty-print-source``) special directive that ``parse_trans`` recognises::

 -pt_pp_src(true).


This will cause a file, ``<Class name>.xfm`` (ex: ``class_Cat.xfm``), to be created at compilation-time, which contains the pretty-printed output of the parse transform, i.e. the actual Erlang code that will be compiled for that class.


In the very uncommon cases where still more information is needed, the ``pt_log_forms`` directive (for ``parse_trans-log-abstract-forms``) can be used::

-pt_log_forms(true).


It results in another file, ``<Class name>.xforms`` (ex: ``class_Cat.xforms``) to be created, which lists the abstract forms resulting from the parse transform.



:raw-latex:`\pagebreak`


Current Stable Version & Download
=================================


Using Stable Release Archive
----------------------------

WOOPER 2.0 is ready to be used and can be downloaded `here <http://sourceforge.net/project/showfiles.php?group_id=158516&package_id=239574>`_ (FIXME).

Either a ``.zip`` or a ``.tar.bz2`` can be retrieved. WOOPER has been fully functional (pun intended!), starting from its 0.1 version.

One way of building all of WOOPER (base files and examples) is, from UNIX or on Windows from a Cygwin or MSYS shell, once the archive is downloaded and extracted, to execute ``make all`` from the WOOPER directory.

For example::

  tar xvjf wooper-x.y.tar.bz2 && cd wooper-x.y && make all

It will build and run all, including the various WOOPER test cases.



Using Cutting-Edge SVN
----------------------

A SVN (anonymous) check-out of WOOPER code can be obtained thanks to, for example::

  svn co https://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/code/scripts/erlang Wooper-code-checkout


We try to ensure that the main line (``trunk``) always stays functional. Evolutions are to be take place in feature branches.


Ceylan developers should used their Sourceforge user name so that they can commit changes::

  svn co --username Your_SF_User_Name https://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/code/scripts/erlang Wooper-code-checkout


Check-out of WOOPER documentation can be performed thanks to::

  svn co --username YourSFUserName https://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/doc/web/main/documentation/wooper Wooper-doc-checkout


If just wanting a SVN anonymous export, use for example::

  svn export http://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/code/scripts/erlang Wooper-code-export

and::

  svn export http://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/doc/web/main/documentation/wooper Wooper-doc-export




:raw-latex:`\pagebreak`


Version History & Changes
=========================

As mentioned previously, there are two flavours of WOOPER, either based on hashtable or on parse transforms.

Here is their mapping to actual released versions:

:raw-html:`<img src="wooper-versions.png"></img>`
:raw-latex:`\includegraphics[scale=0.34]{wooper-versions.png}`


.. contents:: Versions
		 :local:
		 :depth: 1




Version 2.0 [cutting-edge, in SVN only]
---------------------------------------

Not released yet (work-in-progress).


Version 1.1 [next stable]
----------------------------

 - ``remote_synchronisable_new_link/2`` added for parallel yet synchronous creations
 - type specifications added all over the place
 - better output in case of crash, be it in a method, a constructor or a destructor


Version 1.0 [current stable]
----------------------------

This was the latest stable version of the legacy WOOPER branch, which ranges from the 0.x series to the 1.x series. Although now the 2.x series is the recommended one, it does not fully deprecate this branch as some (rather uncommon) use cases might find the mode of operation of the 1.x series, which is based on hashtables rather than on parse transforms, more suitable.

Indeed, contrary to more recent versions, this 1.x series allows for example attributes to be dynamically added and removed (which is, however, usually considered as a bad practise).

The memory footprint of instances of the 1.x series is generally significantly higher, as for the execution durations.

Note finally that instances from either series are fully interoperable.

The latest stable version of that branch can be found in:

 - `wooper-1.0.tar.bz2 <http://downloads.sourceforge.net/ceylan/wooper-1.0.tar.bz2>`_

 - `wooper-1.0.zip <http://downloads.sourceforge.net/ceylan/wooper-1.0.zip>`_

This milestone version is almost exactly the same as the previous 0.4 version.



Version 0.4
-----------

It is mainly a BFO (*Bug Fixes Only*) version, as functional coverage is pretty complete already.

Main changes are:

 - debug mode enhanced a lot: many checkings are made at all fronteers between WOOPER and either the user code (messages) or the class code (constructors, methods, destructor, execute requests); user-friendly explicit error messages are displayed instead of raw errors in most cases; ``is_record`` used to better detect when an expected state is not properly returned

 - ``wooper_result`` not appended any more to method returns in debug mode

 - release mode tested and fixed

 - ``exit`` replaced by ``throw``, use of newer and better ``try/catch`` instead of mere ``catch``

 - destructor chained calls properly fixed this time

 - ``delete_any_instance_referenced_in/2`` added, ``wooper_return_state_*`` macros simplified, ``remote_*`` bug fixed



Version 0.3 [current stable]
----------------------------

Released on Wednesday, March 25, 2009.

Main changes are:

 - destructors are automatically chained as appropriate, and they can be overridden at will

 - incoming EXIT messages are caught by a default WOOPER handler which can be overridden on a per-class basis by the user-specified ``onWooperExitReceived/3`` method

 - direct method invocation supported, thanks to the ``executeRequest`` and ``executeOneway`` constructs, and ``wooper_result`` no more appended to the result tuple

 - synchronous spawn operations added or improved: ``synchronous_new/synchronous_new_link`` and al; corresponding template updated

 - state management enriched: ``popFromAttribute`` added

 - all new variations on remote nodes improved or added

 - major update of the documentation



Version 0.2
-----------

Released on Friday, December 21, 2007. Still fully functional!

Main changes are:

 - the sender PID is made available to requests in the instance state variable (see ``request_sender`` member, used automatically by the ``getSender`` macro)

 - runtime errors better identified and notified

 - macros for attribute management added, existing ones more robust and faster

 - fixed a potential race condition when two callers request nearly at the same time the WOOPER class manager (previous mechanism worked, class manager was a singleton indeed, but second caller was not notified)

 - improved build (Emakefile generated), comments, error output

 - test template added

 - documentation updated



Version 0.1
-----------

Released on Sunday, July 22, 2007. Already fully functional!




:raw-latex:`\pagebreak`


WOOPER Inner Workings
=====================

Each instance runs a main loop (``wooper_main_loop``, defined in `wooper.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper.hrl?view=markup>`_) that keeps its internal state and through a blocking ``receive`` serves the methods as specified by incoming messages, quite similarly to a classical server that loops on an updated state, like in::

  my_server(State) ->
	receive
		{command,{M,P}} ->
  			NewState = f(State,M,P),
			my_server(State)
	end.


In each instance, WOOPER manages the tail-recursive infinite surrounding loop, ``State`` corresponding to the state of the instance, and ``f(State,M,P)`` corresponding to the WOOPER logic that triggers the user-defined method ``M`` with the current state (``State``) and the specified parameters (``P``), and that may return a result.

The per-instance kept state is twofold.



Method Virtual Table
--------------------

General Principle
.................

This table allows, for a given class, to determine which module implements actually each supported method.

For example, all instances of ``class_Cat`` have to know that their ``getWhiskerColor/1`` method is defined directly in that class, as opposed to their ``setAge`` method, whose actual implementation is to be found, say, in ``class_Mammal``, should this class have overridden it from ``class_Creature``.

As performing a method look-up through the entire inheritance graph at each call would waste resources, the look-up is precomputed as much as possible, and as soon as possible.



Implementation In WOOPER version 2.0 and more recent
....................................................

The association from a method name to a module name is done at compilation-time: an appropriate parse transform determines the mapping for each class, and automatically adds it to the generated BEAM.

For example, the ``class_Cat:getWhiskerColor/1`` function is left untouched (except that it is exported, if listed in ``get_member_methods/0``), but  ``class_Cat:setAge/2`` now directly links to ``class_Mammal:setAge/2``, so that the minimal run-time overhead is incurred when a cat instance receives a call to that method.

As a result, this virtual table can be considered as being directly embedded in the code of the module (in the BEAM file), resulting, in terms of memory, in a null per-instance overhead for this virtual table.



Implementation In Hashtable-Based WOOPER versions
.................................................

A per-class hashtable is built at runtime, on the first creation of an instance of this class, and stored by the unique (singleton) WOOPER class manager that shares it to all the class instances.

This manager is itself spawned the first time it is needed, and stays ready for all instances of various classes being created (it uses a hashtable to associate to each class its specific virtual table).

This per-class method table has for keys the known method names (atoms) for this class, associated to the values being the most specialised module, in inheritance tree, that defines that method.

Hence each instance has a reference to a shared hashtable that allows for a direct method look-up.

As the table is built only once and is theoritically shared by all instances [#]_, it adds very little overhead, space-wise and time-wise. Thanks to the hashtable, method look-up is expected to be quite efficient too.

.. [#] Provided that Erlang does not copy these shared immutable structures, which sadly does not seem to be currently the case with the vanilla virtual machine (except for binaries, which are of no use here), inducing a large per-instance overhead which, in turn, reduces a lot the scalability that can be achieved thanks to these WOOPER versions.




Attribute Table
---------------


Implementation In WOOPER version 2.0 and more recent
....................................................

At compile-time, a parse transform reads the attributes declared thanks to the ``get_attributes/0`` function for the current class and, recursively, for all mother classes (direct or not).

Note that, regardless of all qualifiers, a class is not allowed to define an attribute whose name is the same of an inherited one, to prevent collisions (even if both attributes are declared private).

Once having determined the full list of attributes for that class, the parse transform generates a class-specific record to hold them, each attribute being mapped into a first-level field of the record.

Note that even if a record is defined for the mammal class and if the mammal attributes form a subset of the cat ones, the cat record will not include the mammal one: it will define its own record, with all attributes at the same level (no nesting).

Then the state of each of the class instances will be based on these class-specific records, and the state-management functions (like ``setAttribute``) are translated at compile-time (inlined) as statements operating on instances of these records.

The default getter/setter methods are automatically defined for public attributes.



Implementation In Hashtable-Based WOOPER versions
.................................................

This is another hashtable, this time per-instance.

Keys are attribute names, values are attribute values.

It allows a seamless yet efficient access to all data members, including inherited ones.



Implementation
--------------

.. Note:: To be updated.

WOOPER relies only on these specific files:

 - `wooper.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper.hrl?view=markup>`_: the WOOPER core, which gives to the modules using it all the OOP constructs discussed

 - `wooper_class_manager.hrl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper_class_manager.hrl?view=markup>`_: the tiny class manager header

 - `wooper_class_manager.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper/src/wooper_class_manager.hrl?view=markup>`_: the class manager itself, the unique process that is automatically spawned to share virtual tables among instances on a node

 - `hashtable.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/common/hashtable.erl?view=markup>`_: efficient associative table used by WOOPER for virtual tables, methods, attributes

 - `utils.erl <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/erlang/common/utils.erl?view=markup>`_: a small module used by the hashtable and other modules


The latest two come from the ``common`` sub-module.




:raw-latex:`\pagebreak`


Issues & Planned Enhancements
=============================


For all versions of WOOPER:

 - test impact of using HiPE
 - integrate automatic **persistent storage** of instance states into Mnesia databases
 - integrate specific constructs for code reflection
 - check that a class specified in ``execute*With`` is a (direct or not) mother class indeed, at least in debug mode

For WOOPER versions 2.0 and more recent:

 - provide WOOPER constructs to define attributes thanks to the `Builder Design Pattern <http://en.wikipedia.org/wiki/Builder_pattern>`_ and/or factories


For hashtable-based versions of WOOPER:

 - is **wooper_main_loop** (in ``wooper.hrl``) really tail-recursive? I think so

 - would there be a **more efficient** implementation of hashtables? (ex: using proplists, process dictionary, generated modules, dict or ETS-based?); more generally speaking, some profiling could be done to further increase overall performances

 - even when pasting a template, having to declare all the new-related operators (ex: ``new_link/N``) is a bit laborious; maybe an appropriate parse transform could do the trick and automate this declaration?

 - ensure that all instances of a given class *reference* the same hashtable dedicated to the method look-ups, and do not have each their own private *copy* of it (mere referencing is expected to result from single-assignment); some checking should be performed; storing a per-class direct method mapping could also be done with prebuilt modules: ``class_Cat`` would rely on an automatically generated ``class_Cat_mt`` (for "method table") module, which would just be used to convert a method name to the name of the module that should be called in the context of that class, inheritance-wise

 - ensure that each of these references remains purely *local* to the node (no network access wanted for method look-up!); this should be the case thanks to the local WOOPER class manager; otherwise, other types of tables could be used (maybe ETS)

 - add proper support for qualifier-based declarations of methods and attributes (ex: public, protected, private, final, const)





:raw-latex:`\pagebreak`


.. _`open source`:

Licence
=======

WOOPER is licensed by its author (Olivier Boudeville) under a disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:

	- `Mozilla Public License <http://www.mozilla.org/MPL/MPL-1.1.html>`_ (MPL), version 1.1 or later (very close to the `Erlang Public License <http://www.erlang.org/EPLICENSE>`_, except aspects regarding Ericsson and/or the Swedish law)

	- `GNU General Public License <http://www.gnu.org/licenses/gpl-3.0.html>`_ (GPL), version 3.0 or later

	- `GNU Lesser General Public License <http://www.gnu.org/licenses/lgpl.html>`_ (LGPL), version 3.0 or later


This allows the use of the WOOPER code in as wide a variety of software projects as possible, while still maintaining copyleft on this code.

Being triple-licensed means that someone (the licensee) who modifies and/or distributes it can choose which of the available sets of licence terms he is operating under.

Enhancements are expected to be back-contributed, so that everyone can benefit from them.






:raw-latex:`\pagebreak`


Sources, Inspirations & Alternate Solutions
===========================================

 - **Concurrent Programming in Erlang**, Joe Armstrong, Robert Virding, Claes Wikström et Mike Williams. Chapter 18, page 299: Object-oriented Programming. This book describes a simple way of implementing multiple inheritance, without virtual table, at the expense of a (probably slow) systematic method look-up (at each method call). No specific state management is supported

 - Chris Rathman's `approach <http://www.angelfire.com/tx4/cus/shapes/erlang.html>`_ to life cycle management and polymorphism. Inheritance not supported

 - As Burkhard Neppert suggested, an alternative way of implementing OOP here could be to use Erlang behaviours. This is the way OTP handles generic functionalities that can be specialised (e.g. ``gen_server``). One approach could be to map each object-oriented base class to an Erlang **behaviour**. See some guidelines about `defining <http://wiki.trapexit.org/index.php/Defining_Your_Own_Behaviour>`_ your own behaviours and making them `cascade <http://wiki.trapexit.org/index.php/Cascading_Behaviours>`_

 - As mentioned by Niclas Eklund, despite relying on quite different operating modes, WOOPER and `Orber <http://www1.erlang.org/doc/apps/orber/index.html>`_, an Erlang implementation of a **CORBA ORB** (*Object Request Broker*) offer similar OOP features, as CORBA IDL implies an object-oriented approach (see their `OMG IDL to Erlang Mapping <http://www.erlang.org/doc/apps/orber/ch_idl_to_erlang_mapping.html#6>`_)


WOOPER and Orber are rather different beasts, though: WOOPER is very lightweight (less than 2300 lines, including blank lines and numerous comments), does not involve a specific (IDL) compiler generating several stub/skeleton Erlang files, nor depends on OTP or on Mnesia, whereas Orber offers a full CORBA implementation, including IDL language mapping, CosNaming, IIOP, Interface Repository, etc.

Since Orber respects the OMG standard, integrating a new language (C/C++, Java, Smalltalk, Ada, Lisp, Python etc.) should be rather easy. On the other hand, if a full-blown CORBA-compliant middleware is not needed, if simplicity and ease of understanding is a key point, then WOOPER could be preferred. If unsure, give a try to both!


See also another IDL-based approach (otherwise not connected to CORBA), the `Generic Server Back-end <http://www.erlang.org/doc/apps/ic/ch_erl_genserv.html#5>`_ (wrapper around ``gen_server``).


The WOOPER name is also a tribute to the underestimated `Wargames <http://en.wikipedia.org/wiki/WarGames>`_ movie (remember the `WOPR <http://en.wikipedia.org/wiki/WOPR>`_, the NORAD central computer?), which the author enjoyed a lot. It is as well a second-order tribute to the *Double Wooper King Size* (*Whopper* in most if not all countries), which is/was a great hamburger indeed (in France, they are not available any more).



:raw-latex:`\pagebreak`


Support
=======

Bugs, questions, remarks, patches, requests for enhancements, etc. are to be sent to the `ceylan-wooper at lists dot sourceforge dot net <mailto:ceylan-wooper@lists.sourceforge.net>`_.

One must `register <https://lists.sourceforge.net/lists/listinfo/ceylan-wooper>`_ first.




For WOOPER Developers
=====================

When a new WOOPER version is released, tag the corresponding file versions, like in::

  svn copy https://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/trunk/src/code/scripts/erlang/wooper https://ceylan.svn.sourceforge.net/svnroot/ceylan/Ceylan/tags/wooper-release-0.1 -m "First release (0.1) of WOOPER, already fully functional."




Please React!
=============

If you have information more detailed or more recent than those presented in this document, if you noticed errors, neglects or points insufficiently discussed, `drop us a line <mailto:olivier.boudeville@online.fr?subject=[Ceylan]%20WOOPER>`_!
