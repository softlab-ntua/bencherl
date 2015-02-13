% Modular WOOPER header gathering the primitives (exports) to manage the state
% of an instance.


-export([
		 setAttribute/3, setAttributes/2,
		 getAttribute/2,
		 is_wooper_debug/0,
		 hasAttribute/2,
		 removeAttribute/2,
		 addToAttribute/3,
		 subtractFromAttribute/3,
		 incrementAttribute/2,
		 decrementAttribute/2,
		 toggleAttribute/2,
		 appendToAttribute/3,
		 deleteFromAttribute/3,
		 addKeyValueToAttribute/4,
		 popFromAttribute/2,
		 wooper_check_undefined/2

		]).



% Macros versus functions.


% Note that the below macros had to be one-liners, otherwise their actual value
% would be the first evaluated in the macro - not the last.
%
% Indeed 'NewState = ?setAttribute( State, ...)' *and* ' my_function(
% ?setAttribute( State, ...)' have to be supported, whereas they respectively
% use the first and the last element of a sequence of statements separated with
% a comma.
%
% Indeed, if using a macro with multiple instructions, in the first example
% NewState would be bound to the result of the first instruction of the macro,
% as shown in:

% 1> A=a,b,c.
% c
% 2> A.
% a

% instead of the expected final macro result, whereas for the second example the
% result sent to my_function would be (as expected) the updated state (c, in the
% previous shell output).

% Another problem that must be avoided is that if calling for example
% '?setAttribute( f(State), name, value )', then we want any side-effect caused
% by f/1 to be triggered exactly once.
%
% For example in the setAttribute/3 macros defined below (and now
% commented-out), multiple references to (State) would have resulted in as many
% calls to f/1, which is not correct.

% Therefore we came to the conclusion that macros could not fulfill our needs,
% and we defined full-blown functions instead, even if we incur the performance
% penalty of an additional function call. To alleviate this overhead, we
% recommend the compiler to inline these functions.
%
% So finally '?setAttribute( AState, name, value )' should be transformed into
% 'setAttribute( AState, name, value )'.

% Newer macros are defined for backward compatibility, resulting in the same
% function call. They should not be used anymore, their purpose is just to
% support unchanged legacy code.

% Finally, with an upcoming version of WOOPER making use of parse transforms and
% better state storage, attributes will be used thanks to the same function
% calls, while fully removing their performance penalty.



% Macro-based implementations (except the getAttr/1 and getSender/0 macros) and
% legacy macros could be removed in next versions.



% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause error.
%
% This macro is usually more useful than the getAttribute function, as one
% generally wants to retrieve an attribute already available in the 'State'
% parameter of a method (otherwise that value is available through a bound
% variable in the method body).
%
% Therefore the use of a variable named 'State' can often be implied.
%
% Beware to the implicit use of the 'State' variable: in some cases other states
% should be used; due to this implicit variable, getAttr must remain a macro.
%
% See the longer getAttribute/2 function.
%
% Definitively not a legacy macro.
%
-define( getAttr(AttributeName),
	getAttribute( State, (AttributeName) )
).



% Returns the sender of the request.
%
% Must be a macro, due to the implied 'State' variable.
%
-define( getSender(), State#state_holder.request_sender ).



% Checks that the value of specified attribute is 'undefined'.
% Triggers an exception otherwise.
%
% Note: operates on a state called 'State', thus must be a macro.
%
% The check could be disabled in debug mode.
%
% This results in function call, as a pure macro, if used more than once in a
% function, would trigger warnings about unused variables or, more probably,
% would attempt to perform unintended pattern matches.
%
-define( checkUndefined( Attribute ),
	wooper_check_undefined( State, (Attribute) )
).




% Uncomment if old-style attribute management macros are to be enabled:
% (by default we do not want to support them anymore)
%-define(use_legacy_macros,).


-ifdef( use_legacy_macros ).

% These macros are defined (if enabled) for backward compatibility only:



% Faulty macro (risk of side-effects being executed more than once):

% Sets specified attribute of the instance to the specified value, based from
% specified state.
%
% Returns an updated state.
%
% Always succeeds.
%
% See also: the setAttributes macro to set more than one attribute at a time.
%
%-define( setAttribute( State, AttributeName, AttributeValue ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntry(
%			(AttributeName),
%			(AttributeValue),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( setAttribute( State, AttributeName, AttributeValue ),
	setAttribute( (State), (AttributeName), (AttributeValue) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Sets a list of attribute/value pairs in specified state.
%
% The expected parameter is a list of pairs (2-element tuples), each pair
% containing in first position the attribute name and in second one the
% attribute value.
%
% Returns an updated state.
%
% Always succeeds.
%
% See also: setAttribute/3.
%
%-define( setAttributes( State, ListOfAttributePairs ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntries(
%			(ListOfAttributePairs),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( setAttributes( State, ListOfAttributePairs ),
	setAttributes( (State), (ListOfAttributePairs) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Tells whether specified attribute exists, returns true or false.
%
% Note: usually the best practise is to set all possible attributes from the
% constructor, either to an appropriate value or to 'undefined', instead of
% having instances with or without a given attribute.
%
% Note: not expected to be ever used, as all attributes should be defined
% directly in the constructor, hence no attribute could appear later, if this
% good practise is respected.
%
%-define( hasAttribute( State, AttributeName ),
%	?wooper_hashtable_type:hasEntry( (AttributeName),
%		(State)#state_holder.attribute_table ) ).


% Macro defined for backward compatibility only:

-define( hasAttribute( State, AttributeName ),
	hasAttribute( (State), (AttributeName) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause crash.
%
% Note: almost never used, as either the attribute can be obtained with
% getAttr/1 (as externally defined) or it is already bound to a variable.
%
% See also: the getAttr/1 shorthand.
%
%-define( getAttribute( State, AttributeName ),
%	?wooper_hashtable_type:getEntry( (AttributeName),
%		(State)#state_holder.attribute_table ) ).


% Macro defined for backward compatibility only:

-define( getAttribute( State, AttributeName ),
	getAttribute( (State), (AttributeName) ) ).



% Faulty macro (risk of side-effects being executed more than once):

% Returns an updated state not having anymore specified attribute.
%
% No error is triggered if the specified attribute was not existing.
%
%-define( removeAttribute( State, AttributeName ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:removeEntry( (AttributeName),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( removeAttribute( State, AttributeName ),
	removeAttribute( (State), (AttributeName) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Adds specified value to specified attribute, supposed to be a number.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%-define( addToAttribute( State, AttributeName, Value ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addToEntry(
%			(AttributeName),
%			(Value),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( addToAttribute( State, AttributeName, Value ),
	addToAttribute( (State), (AttributeName), (Value) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Subtracts specified value from specified attribute, supposed to be a number.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no subtraction can be performed on the attribute value.
%
%-define( subtractFromAttribute( State, AttributeName, Value ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:subtractFromEntry(
%			(AttributeName),
%			(Value),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( subtractFromAttribute( State, AttributeName, Value ),
	subtractFromAttribute( (State), (AttributeName), (Value) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Returns an updated state in which specified boolean attribute is toggled:
% if true will be false, if false will be true.
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
%-define( toggleAttribute( State, BooleanAttributeName ),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:toggleEntry(
%			(BooleanAttributeName),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( toggleAttribute( State, BooleanAttributeName ),
	toggleAttribute( (State), (BooleanAttributeName) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
%
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
%
%-define( appendToAttribute( State, AttributeName, Element ),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:appendToEntry(
%			(AttributeName),
%			(Element),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define(appendToAttribute( State, AttributeName, Element ),
	appendToAttribute( (State), (AttributeName), (Element) )
).



% Faulty macro (risk of side-effects being executed more than once):


% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
%
% A case clause is triggered if the attribute did not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
%-define( deleteFromAttribute( State, AttributeName, Element ),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:deleteFromEntry(
%			(AttributeName),
%			(Element),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( deleteFromAttribute( State, AttributeName, Element ),
	deleteFromAttribute( (State), (AttributeName), (Element) )
).



% Faulty macro (risk of side-effects being executed more than once):

% Assumes the specified attribute is a hashtable and adds the specified
% key/value pair to it.
%
% Several lines compacted into a bit impressive one-liner.
%
%-define( addKeyValueToAttribute( State, AttributeName, Key, Value ),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntry(
%			(AttributeName),
%			?wooper_hashtable_type:addEntry( (Key), (Value),
%				?wooper_hashtable_type:getEntry( (AttributeName),
%					(State)#state_holder.attribute_table ) ),
%			(State)#state_holder.attribute_table )
%	}
%).


% Macro defined for backward compatibility only:

-define( addKeyValueToAttribute( State, AttributeName, Key, Value ),
	addKeyValueToAttribute( (State), (AttributeName), (Key), (Value) )
).



% Faulty macro (risk of side-effects being executed more than once):


% Removes the head from specified attribute, supposed to be a list, and returns
% a tuple { NewState, PoppedHead }.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{ PoppedState, Head } = ?popFromAttribute( State, my_list )'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
% Note: This cannot be a one-line macro, it has to be a function.
%
%-define( popFromAttribute( State, AttributeName ),
%	wooper_pop_from_attribute( (State), (AttributeName) )
%).


% Macro defined for backward compatibility only:

-define( popFromAttribute( State, AttributeName ),
	popFromAttribute( (State), (AttributeName) )
).



-endif. % use_legacy_macros
