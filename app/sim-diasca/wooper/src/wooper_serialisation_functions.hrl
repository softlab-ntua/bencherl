% Modular WOOPER header gathering all serialisation-related functions.



% Note: because of the hooks (which must be class-specific) and of the lack of a
% common ancestor to all WOOPER classes (we could have defined methods like
% serialise/3 and all in this module), some serialisation services are provided
% through an header file (this one), short of being able to be defined in
% wooper_serialisation.erl (which would be preferable).



% Serialises the specified instance (i.e. the state thereof), using specified
% entry transformer and user data.
%
% Returns a binary corresponding to a { InnerPair, UpdatedUserData } pair made
% of:
%
% 1. an inner pair { Classname, Entries }, converted into a binary and
% containing the corresponding class name (as an atom) and the associated
% serialisation (as a list of transformed terms)
%
% 2. the resulting user-data (possibly modified by the successive operations
% done by the entry transformer)
%
% This is a method, but as it is defined unconditionally in the WOOPER header
% (in the future, a WOOPER object superclass will be available, defining a
% default, overridable serialise/3 method), it cannot be overridden on a
% per-class basis. Hence the needs for serialisation hooks. We still need to
% specify the classname however, as at deserialisation time this information
% will be necessary.
%
% Notes:
%
% - we do not take the 'request_sender' field into account for serialisation,
% as, by design, there is indeed such a caller (since serialise/3 is a request),
% but it is of no interest here
%
% - as we do not have a typed (class-specific) state (record-like)
% data-structures, we have to store not only the values, but also the keys; a
% major progress in terms of compactness still lies there
%
% (const request)
%
-spec serialise( wooper:state(), wooper_serialisation:entry_transformer(),
				 basic_utils:user_data() ) -> request_return(
	   { wooper_serialisation:bin_serialisation(), basic_utils:user_data() } ).
serialise( State, _EntryTransformer=undefined, UserData ) ->

	% Here no entry transformer is to be used, raw serialisation.

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{ attribute_table=AttributeTable,
					actual_class=Classname } = pre_serialise_hook( State ),

	io:format( " - serialising, with no transformer, instance ~p of class ~s~n",
			  [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc.
	%
	% The WOOPER serialisation mechanisms do not account for them currently (ex:
	% links may be dictated by the application logic and thus may not have to be
	% stored), except one: the current random state of the serialised process,
	% which is transparently managed by WOOPER so that the deserialisation will
	% lead to restoring the right random state.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },


	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort( [ RandomAttribute |
					   ?wooper_hashtable_type:enumerate( AttributeTable )  ] ),

	% By default returns { Classname, Entries }:
	FullContent = post_serialise_hook( Classname, Entries, PreState ),

	SerialisedContent = term_to_binary( FullContent,
										_Opts=[ { compressed, 9 } ] ),

	Res = { SerialisedContent, UserData },

	% Yes, it is 'State', as we do not want to continue with any state forged
	% for the serialisation (ex: with transformed local processes), we want to
	% continue as we were!
	%
	?wooper_return_state_result( State, Res );



serialise( State, EntryTransformer, UserData ) ->

	% Here an entry transformer is to be used, for a smarter serialisation (ex:
	% PID-aware).

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{ attribute_table=AttributeTable,
					actual_class=Classname } = pre_serialise_hook( State ),

	io:format( " - serialising, with transformer, instance ~p of class ~s~n",
			  [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc.
	%
	% The WOOPER serialisation mechanisms do not account for them currently (ex:
	% links may be dictated by the application logic and thus may not have to be
	% stored), except one: the current random state of the serialised process,
	% which is transparently managed by WOOPER so that the deserialisation will
	% lead to restoring the right random state.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },

	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort( [ RandomAttribute |
					   ?wooper_hashtable_type:enumerate( AttributeTable )  ] ),

	%io:format( "Original entries:~n~p~n", [ Entries ] ),

	% Applying the entry transformer on each of them:
	{ TransformedEntries, FinalUserData } = lists:foldl( EntryTransformer,
					  _Acc0={ _ResultingEntries=[], UserData }, _List=Entries ),

	%io:format( "Transformed entries:~n~p~n", [ TransformedEntries ] ),

	% No need to reverse the transformed list.

	% By default returns { Classname, TransformedEntries }:
	FullContent = post_serialise_hook( Classname, TransformedEntries,
									   PreState ),

	SerialisedContent = term_to_binary( FullContent,
										_Opts=[ { compressed, 9 } ] ),

	Res = { SerialisedContent, FinalUserData },

	% Yes, it is 'State', as we do not want to continue with any state forged
	% for the serialisation (ex: with transformed local processes), we want to
	% continue as we were before the serialisat
	%
	?wooper_return_state_result( State, Res ).




% Hooks section.



% For some classes, the implementor may want to define pre/post hooks for
% serialisation/deserialisation.
%
% In this case he should define 'wooper_serialisation_hooks' and specify the
% four corresponding hooks.
%
-ifndef(wooper_serialisation_hooks).



% Default do-nothing hooks:


% Triggered just before serialisation.
%
% We are here to return a state directly suitable for serialisation, for example
% with no transient technical identifiers (like PID, open files, etc.) - unless
% a later entry transformer is able to manage them.
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->
	State.



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
% (we do not want to return a state, as we do not want that a state modified by
% the serialisation be mistakenly used afterwards)
%
-spec post_serialise_hook( class_name(),
	   wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
% Default version corresponding to post_serialise_hook/3.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	State.


-endif. % wooper_serialisation_hooks
