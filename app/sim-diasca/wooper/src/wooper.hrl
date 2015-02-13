% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% WOOPER: Wrapper for OOP in ERlang.

% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date: Friday, July 6, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL, see:
% http://ceylan.sourceforge.net/main/documentation/wooper/index.html#license


% Provides most classical constructs: new/delete operators, remote method
% invocation (RMI), polymorphism and multiple inheritance, all with state
% management and in a quite efficient way (i.e. no significantly faster approach
% in Erlang could be imagined by the author - before he became aware of the
% existence of parse transforms).

% Instances are created thanks to the new operator, which calls automatically
% the relevant constructor ('construct' function).

% A class C is mapped to an Erlang module, preferably named 'class_C'.
%
% An active object is mapped to an Erlang process.
%
% Methods support Remote Invocation Calls, mapped to Erlang messages.
%
% Inheritance is implemented thanks to a per-class method virtual table,
% including the locally-defined ones and all the inherited ones.
%
% This table is shared among all the instances of a given class, thanks to a
% singleton-like class manager process that keeps references to the virtual
% table of each class.
%
% Instance state is maintained thanks to a per-instance attribute table, storing
% all its attributes, including all the inherited ones.
%
% The hashtable type, defined in hashtable.erl, is used at all levels:
% per-instance (for the attribute table), per-class (for the so-called virtual
% table), per-node (for the class manager).
%
% The proplist module could be used instead.

% When an exported function is called as a method (i.e. it is listed in the
% wooper_method_export variable, see below) the list of parameters being
% received is prefixed with the instance state (a bit like 'self' in Python):
% A ! { aMethod, [1,2] } results in the calling of the 'aMethod' function
% defined in the class module of A (exported thanks to wooper_method_export)
% with parameters automatically given to that function being: 'CurrentStateOfA,
% 1, 2' instead of '1, 2', with CurrentStateOfA being the A state variable
% automatically kept in the instance WOOPER main loop.
%
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2 (it
% is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is called
% with only two parameters specified (state not being included).


% The usual content of the '-export([XXX]).' clause in a class module should be
% dispatched in:
%
%    '-define( wooper_method_export, YYY ).', to declare methods, ex:
% '-define( wooper_method_export, getAge/1, setAge/2, declareBirthday/1 ).'
% Zero arity is not possible since there is at least the 'State' first
% parameter. So one just increments the number of intended real
% function-specific parameters in this export.
% Ex: a function 'setAge' taking in input only one logical parameter, NewAge,
% should actually be defined as 'setAge(State,NewAge) -> [..]' and therefore
% declared as: '-define( wooper_method_export, a/1, setAge/2, b/2 ).'
% Note: one should not forget, when overloading a method F/A, to specify it in
% wooper_method_export, otherwise its closest ancestor method will be called
% instead. In this case a warning is issued at compilation of the child class:
% 'Warning: function F/A is unused.'; static methods can be declared also here.
%
% '-define( wooper_construct_export, new/p, new_link/p, construct/p+1, ...).'
% Ex:
% '-define( wooper_construct_export, new/2, new_link/2, construct/3, ...).'
% to declare the appropriate construction-related functions (the 'new'
% variations and the 'construct' operator), p being the number of
% parameters defined in the wooper_construct_parameters variable.
% Only the relevant 'construct' function has to be actually defined by the
% developer: all new variations are automatically defined appropriately
% (see in this file).
% Declaring and implementing a toString/1 method is optional, but may be
% convenient for the debugging of method implementations.
%
%   '-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
% functions, that are not methods.
%
% Note that the dispatching of functions into wooper_method_export,
% wooper_construct_export and classical exports is done mainly for
% self-documenting purpose (they are all just translated into the usual
% export declarations).



% A note about the MODULE macro:
%
% Its behaviour, similar to a global variable, induces issues, for example when
% a process has to embody a new instance, thanks to the wooper module: ?MODULE
% would then be 'wooper' instead of the one of the targeted class.
%
% For this reason, we shall always rely only onto the 'actual_class' of the
% state_holder record (rather than on ?MODULE).
%
% When such a state is not available, we are before the creation of the instance
% (in a *new* operator), and we are in the code path for normal instance
% creation where ?MODULE can be used.
%
% Otherwise an embodiment is performed, in which case the class name is a
% parameter, and ?MODULE shall not be used.



% Regarding includes:
%
% The unability to export functions or types after function definitions is a
% real pain when having multiple headers doing both.
%
% In case of problem, we split a header H.hrl into H_exports.hrl and
% H_functions.hrl, and include them only in the final module to compile (as
% opposed to having each module list the headers it depends on), into two
% sections, first the exports, then the function definitions. This also allows
% to have each relevant header included exactly once (hence, for example, no
% need for include guards).


% First exports:

-include("wooper_classes_exports.hrl").
-include("wooper_defines_exports.hrl").
-include("wooper_destruction_exports.hrl").
-include("wooper_execute_exports.hrl").
-include("wooper_execute_internal_exports.hrl").
-include("wooper_serialisation_exports.hrl").
-include("wooper_state_exports.hrl").
-include("wooper_types_exports.hrl").
-include("wooper_main_loop_exports.hrl").


% Then function definitions:

-include("wooper_classes_functions.hrl").
-include("wooper_creation_functions.hrl").
-include("wooper_destruction_functions.hrl").
-include("wooper_execute_functions.hrl").
-include("wooper_execute_internal_functions.hrl").
-include("wooper_main_loop_functions.hrl").
-include("wooper_serialisation_functions.hrl").
-include("wooper_state_functions.hrl").
