#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable


% Copyright (C) 2010-2012 EDF R&D

% This file is part of Sim-Diasca, and has been an accepted as a contribution to
% the Ceylan Erlang library, in this 'Common' package.



% This script will process recursively all BEAM files from specified directory
% and add to each function the type specification that could be deduced from its
% current implementation.

% That way, user code may have a better chance thanks to dialyzer to statically
% detect mistakes, and regressions that may appear because of later changes will
% be better spotted.

% Note of course that the added type specifications are not the ones intended by
% the original developer (whose mind cannot be read by this script), but the
% ones deduced from the current code, which may be finer or coarser.


% An hypothesis here is that when scanning a ${path}/X.beam file, the
% corresponding source file is to be found in ${path}/X.erl.

% All functions are expected to be declared at the beginning of a line (no
% leading whitespaces).

% A useful feature to be added would be that running this script multiple times
% against the same file writes only up to once each specification (currently
% each pass adds all type specifications blindly). Moreover, if the user had
% already written a type spec, then it should be kept as is, instead of being
% added or replacing the former version.




% The key used to decipher the BEAM files, if needed:
%
% (must match the debug_info_key in ERLANG_COMPILER_OPT, see
% common/GNUmakevars.inc)
-define( beam_key, "Ceylan-common" ).


get_usage() ->
	"   Usage: add-deduced-type-specs.escript ELEMENT\n\n"
	"   Adds, for each selected BEAM file (either specified directly "
	"as a file, "
	"or found recursively from a specified directory), "
	"in the corresponding source file(s), for each function, "
	"the type specification that could be deduced "
	"from its current implementation.\n\n"
	"   ELEMENT is either the path of a BEAM file or a directory "
	"that will be scanned recursively for BEAM files.\n"
	"   Note that BEAM files must be already compiled, and "
	"with debug information (see the '+debug_info' compile flag)."
	"\n".



% We want this escript to be standalone, thus we copy here "verbatim" (module
% references fixed) the functions of interest, so that there is no prerequisite.


% Taken from common/src/utils/text_utils.erl:

% join(Separator,ListToJoin), ex: join( '-', [ "Barbara", "Ann" ] ).
%
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation.
%
% Inspired from http://www.trapexit.org/String_join_with.
%
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
%
% Note: use string:tokens to split the string.
join(_Separator,[]) ->
	"";

join(Separator,ListToJoin) ->
	lists:flatten( lists:reverse( join(Separator, ListToJoin, []) ) ).


join(_Separator,[],Acc) ->
	Acc;

join(_Separator,[H| [] ],Acc) ->
	[H|Acc];

join(Separator,[H|T],Acc) ->
	join(Separator, T, [Separator,H|Acc]).


% End of the common/src/utils/text_utils.erl section.





% Taken from common/src/utils/file_utils.erl:


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Tells whether specified file entry exists, regardless of its type.
exists(EntryName) ->
	case file:read_file_info(EntryName) of

		{ok,_FileInfo} ->
			true;

		{error,_Reason} ->
			false

	end.



% Returns the type of the specified file entry, in:
% device | directory | regular | other.
get_type_of(EntryName) ->

	case file:read_file_info(EntryName) of

		{ok,FileInfo} ->
			#file_info{ type=FileType } = FileInfo,
			FileType;

		{error,eloop} ->
			% Probably a recursive symlink:
			throw({too_many_symlink_levels,EntryName});

		{error,enoent} ->
			throw({non_existing_entry,EntryName})

	end.


% Returns whether the specified entry exists and is a directory.
%
% Returns true or false, and cannot trigger an exception.
is_existing_directory(EntryName) ->
	case exists(EntryName) andalso get_type_of(EntryName) of

		directory ->
			true ;

		_ ->
			false

	end.


% End of the common/src/utils/file_utils.erl section.



% Returns the list of {FunctionName,Arity} pairs that are allowed to be found in
% the BEAM but not in the sources.
%
% Notably useful for function definitions inserted either from header files or
% through a parse transform.
%
get_spec_suppressions() ->
	get_wooper_spec_suppressions() ++ get_trace_spec_suppressions().


% All spec suppressions induced by WOOPER.
% These are to be found in wooper.hrl (versions 1.x).
get_wooper_spec_suppressions() ->
	[ {"construct",any}, {"is_wooper_debug",0}, {"new",any}, {"new_link",any},
	  {"popFromAttribute",2}, {"remote_new",any}, {"remote_new_link",any},
	  {"remote_synchronous_new",any}, {"remote_synchronous_new_link",any},
	  {"remote_synchronous_timed_new",any},
	  {"remote_synchronous_timed_new_link",any}, {"removeAttribute",any},
	  {"wooper_receive",0}, {"wooper_get_virtual_table_description",1},
	  {"wooper_get_state_description",1}, {"wooper_get_instance_description",1},
	  {"wooper_get_all_attributes",1}, {"wooper_display_virtual_table",1},
	  {"wooper_display_state",1}, {"wooper_display_loop_state",1},
	  {"wooper_display_instance",1}, {"wooper_destruct",1},
	  {"wooper_debug_listen",3},
	  {"wooper_construct_and_run_synchronous",2},
	  {"wooper_construct_and_run",1},
	  {"wooper_check_undefined",2}, {"toggleAttribute",2},
	  {"synchronous_timed_new_link",any}, {"synchronous_timed_new",any},
	  {"synchronous_new_link",any}, {"synchronous_new",any},
	  {"subtractFromAttribute",3}, {"setAttributes",2}, {"setAttribute",3},
	  {"appendToAttribute",3}, {"addToAttribute",3},
	  {"addKeyValueToAttribute",4}, {"deleteFromAttribute",3},
	  {"delete_any_instance_referenced_in",2},
	  {"executeOneway",2}, {"executeOneway",3}, {"executeOnewayWith",3},
	  {"executeOnewayWith",4}, {"executeRequest",2}, {"executeRequest",3},
	  {"executeRequestWith",3}, {"executeRequestWith",4}, {"getAttribute",2},
	  {"get_class_name",0}, {"get_class_name",1}, {"get_superclasses",0},
	  {"get_superclasses",1}, {"hasAttribute",2} ].



% All spec suppressions induced by the 'Traces' package.
get_trace_spec_suppressions() ->
	[ {"test_receive",0}, {"test_failed",1},
	  {"app_receive",0}, {"app_failed",1} ].



% Entry point of the script.
main( [ "-h" ] ) ->
	io:format( "~s", [get_usage()] );

main( [ "--help" ] ) ->
	io:format( "~s", [get_usage()] );

main( [Element] ) ->

	case exists(Element) of

		true ->

			case get_type_of(Element) of

				directory ->
					manage_dir(Element);

				regular ->
					% One-element list, for homogeneity:
					[manage_file(Element)];

				Other ->
					io:format( "   Error, element '~s' exists, but is neither "
							  "a file nor a directory (~s).~n~s",
							  [Element,Other,get_usage()] ),
					throw( {unexpected_element,Element,Other} )

			end;

		false ->

			io:format( "   Error, element '~s' could not be found.",
					  [Element] ),

			throw( {element_not_found,Element} )

	end;

main( _ ) ->
	io:format( "~n   Error, exactly one parameter should be specified.~n~n~s",
			  [ get_usage() ] ).



% Tells whether a source file in which type specs should be added should be
% backuped first (with a .specbak extension).
do_backup() ->
	true.


% Tells whether verbose outputs are wanted.
is_verbose() ->
	false.



get_all_beams_from(Dir) ->

	case is_existing_directory( Dir ) of

		false ->
			throw( {non_existing_input_directory,Dir} );

		true ->
			ok

	end,

	FileFun = fun( File, Acc ) -> [File|Acc] end,

	AllBeamFiles = filelib:fold_files( Dir, _RegExp=".beam" ++ [$$],
		  _Recursive=true, FileFun, _AccIn=[] ),

	case AllBeamFiles of

		[] ->
			throw( {no_beam_file_to_process_from,Dir} );

		_ ->
			AllBeamFiles

	end.



% Adds type specifications recursively from the specified directory.
manage_dir( Dir ) ->

	AllBeamFiles = case get_all_beams_from(Dir) of

		[] ->
			throw( {no_beam_file_to_process_from,Dir} );

		L ->
			L

	end,

	io:format( "~nWill operate on all BEAM files found from ~s:~n~p~n~n",
			  [ Dir, AllBeamFiles ] ),

	[ manage_file(F) || F <- AllBeamFiles ].




% Generates type specifications for the specified BEAM file, and writes them on
% file.
manage_file( BeamPath ) ->

	BeamDir = filename:dirname( BeamPath ),
	BeamFilename = filename:basename( BeamPath ),

	case filename:extension(BeamFilename) of

		".beam" ->
			ok ;

		Other ->
			io:format( "~nError, a BEAM file (extension: '.beam') is expected, "
					   "whereas target file is '~s' (extension: '~s').~n~n",
					  [BeamFilename,Other] ),

			throw( {not_a_beam_file,BeamFilename,Other} )


	end,

	io:format( " - managing BEAM file '~s'~n", [BeamPath] ),

	% One key to rule all BEAMs:
	beam_lib:crypto_key_fun( fun(init) -> ok; (_) -> ?beam_key end ),

	% 'Plt' means 'Persistent Lookup Table':
	PltFilename = "/tmp/" ++ BeamFilename ++ ".plt",

	Options = [ {files,[BeamPath]}, {output_plt,PltFilename},
			   {analysis_type,'plt_build'} ],

	%io:format( "#### Build plt: ~p.~n~n", [Options] ),

	% Similar to: dialyzer -c m.beam --build_plt --output_plt /tmp/m.beam.plt
	try dialyzer:run(Options) of

		Res ->
			interpret_dialyzer_message( Res, BeamPath )

	catch throw:{dialyzer_error,Thrown} ->

			% If the abstract code is not found, it is either because the BEAM
			% is not compiled with debug_info, or it is compiled so, but without
			% a key matching the beam_key defined in this file.

			io:format( "~nError, dialyzer run failed for ~s: ~s (or is it the "
					  "debug key in this script that does not match the "
					  "BEAM one, or a BEAM not compiled "
					  "with debug information?)~n~n", [BeamPath,Thrown] ),

			throw( {dialyzer_run_failed,BeamPath,Thrown} )

	end,

	integrate_info_from_plt( PltFilename, BeamDir ).



% Notifies dialyzer warnings.
interpret_dialyzer_message( [], _BeamFile ) ->
	ok;

interpret_dialyzer_message( [ W={warn_callgraph,{File,Index},
		   {call_to_missing,[Module,delete,1]} } | T ], BeamFile ) ->

	% With WOOPER, delete/1 is used if defined in the class, otherwise WOOPER
	% manages it by itself.

	% Dialyzer does not see that '?MODULE:delete(State)' is triggered iff such a
	% function exists (thanks to a case with 'lists:member( {delete,1},
	% module_info(exports) )'), so the corresponding warning should be
	% suppressed silently.

	case string:str( File, "wooper.hrl" ) of

		0 ->
			notify( io_lib:format( "~n#### Warning when processing ~s: "
					   "~s:delete/1 called from ~p (line ~B), "
					   "whereas was never defined; dialyzer says: '~s'~n",
					   [BeamFile,Module,File,Index,
						dialyzer:format_warning(W)] ) );

		_ ->
			notify( io_lib:format(
					  "~n(suppressed warning when processing ~s about correct "
					  "destructor ~s:delete/1)~n~n", [BeamFile,Module] ) )

	end,
	interpret_dialyzer_message( T, BeamFile );

interpret_dialyzer_message( [ {warn_callgraph,{File,Index},
		   {call_to_missing,[Module,Function,Arity]} } | T ], BeamFile ) ->
	io:format( "~n#### Warning when processing ~s: "
			   "~s:~s/~B called from ~s (line ~B), "
			   "whereas was never defined.~n~n",
			   [BeamFile,Module,Function,Arity,File,Index] ),
	interpret_dialyzer_message( T, BeamFile );

interpret_dialyzer_message( [H|T], BeamFile ) ->
	io:format( "~n#### Warning when processing ~s:~n~p.~n", [BeamFile,H] ),
	interpret_dialyzer_message( T, BeamFile ).



% Reads type specifications from the dialyzer-produced Plt file, and integrates
% them in the corresponding source file, at the relevant locations (hopefully).
integrate_info_from_plt( PltFilename, BeamDir ) ->

	%io:format( " - reading specifications from plt file '~s'~n",
	%		  [PltFilename] ),

	{Plt,_Info} = dialyzer_plt:plt_and_info_from_file(PltFilename),

	% Contains the deduced type specifications as basic terms:
	%io:format( "~n#### Plt: ~p.~n~n", [Plt] ),

	% Looks like path, dict of module dependencies, etc.:
	%io:format( "~n#### Information: ~p.~n~n", [Info] ),

	% A text with a module header and the list of specs (all in one string).
	% Ex:
	% """
	%
	%%% ------- Module: file_utils -------
	%
	% -spec close(_) -> any()
	% -spec close(_,'overcome_failure' | 'throw_if_failed') -> any()
	% -spec convert_to_filename([any()]) -> any()
	% [...]
	% """
	%
	SpecString = dialyzer_plt:get_specs(Plt),
	%io:format( "~n#### Specification string:~n~s.~n~n", [SpecString] ),

	% Just split the module name (new first element) from the specs:
	_R = [_|SlicedSpecs] = split_module_and_specs(SpecString),
	%io:format( "~n#### Split specifications:~n~s~n~n", [R] ),
	% Ex: [ "-spec close(_) -> any()",
	% "-spec close(_,'overcome_failure' | 'throw_if_failed') -> any()" ]
	write_specs( SlicedSpecs, BeamDir ).


% Allows to remove the module header for dialyzer output.
split_module_and_specs(Specs) ->
	re:split( Specs, "%% ------- Module: (.*) -------", [ {return,list} ] ).



% Manages the writing back of the source file, with specifications added.
write_specs( AllSpecs, Dir ) ->

	[ModuleName|T] = AllSpecs,
	[ModuleSpecs|Rest] = T,

	%io:format( "#### Module name: '~s'~n~n", [ModuleName] ),
	%io:format( "#### Module specs: ~s~n~n", [ModuleSpecs] ),

	case Rest of

		[] ->
			ok;

		_ ->
			io:format( "#### Warning: unexpected content after retrieved "
					  "specifications: ~s.~n~n", [Rest] )

	end,

	% Now, to write back these specs, we have to locate the target source file
	% and find the relevant location to write each of them.

	% There are a number of possible better ways to do this, use module_info,
	% but then we need to -pz all folders, filename:find_src might also work,
	% perhaps even looking at the abstract code also find the line number for
	% that function.

	ErlPath = find_source_file_for( ModuleName, Dir ),

	%io:format( " - inserting specs into source file '~s'~n", [ErlPath] ),

	perform_any_backup( ErlPath ),

	% Read file:

	{ok,Device} = file:open( ErlPath, [read,write] ),
	Lines = read_all_lines( Device, _Acc=[] ),

	FlattenLines = lists:flatten(Lines),

	% Split and filter ModuleSpecs:
	ModuleSplit = re:split( ModuleSpecs, "\n", [ {return,list} ] ),
	%io:format( "ModuleSplit =~n~s", [ModuleSplit] ),

	% List of strings containing type specs:

	NotEmptyFun = fun(X) -> X /= [] end,
	SlicedModuleSpecs = lists:filter( NotEmptyFun, ModuleSplit ),

	%io:format( "~n#### Processed type specifications: ~p.~n~n",
	%		[SlicedModuleSpecs] ),

	% We must first find out where to insert this spec, i.e. determine where the
	% definition of the corresponding function lies.
	%
	% For that, we will make two lists:
	%
	%  - based on these specs returned by dialyzer, a list of
	%  {FunctionName,Arity,Spec} entries, SpecEntries
	%
	%  - based on the parsed source file, a list of {FunctionName,Arity,Index}
	%  entries where Index is the byte count (in the whole source file)
	%  corresponding to the function definition, FunEntries

	SpecEntries = build_spec_entries( SlicedModuleSpecs ),
	%io:format( "SpecEntries = ~p~n", [SpecEntries] ),

	FunctionNames = get_function_names( SpecEntries ),
	%io:format( "FunctionNames = ~p~n", [FunctionNames] ),

	FunEntries = build_fun_entries( FunctionNames, FlattenLines ),
	%io:format( "FunEntries = ~p~n", [FunEntries] ),

	NewSource = insert_spec( FlattenLines, FunEntries, SpecEntries ),

	%io:format( "~n#################################~nNew source:~n~n~s",
	%		   [NewSource] ),

	% Write specifications to file:
	ok = file:write_file( ErlPath, NewSource ).



% Returns a list of all the different function names in the specified entries.
get_function_names( SpecEntries ) ->
	AllNames = [ Name || {Name,_Arity,_Spec} <- SpecEntries ],
	uniquify( AllNames ).

uniquify(List) ->
	sets:to_list( sets:from_list(List) ).

% Returns a list of {FunctionName,Arity,Spec} entries.
build_spec_entries( Specs ) ->
	build_spec_entries( Specs, _Acc=[] ).

 build_spec_entries( _Specs=[], Acc ) ->
	Acc;

build_spec_entries( [Spec|T], Acc ) ->

	%io:format( "~n - building spec entry for ~s~n", [Spec] ),

	Split = re:split( Spec, "-spec (.+)(\s*)->", [ {return,list} ] ),
	[ _ | [FunctionHead|_] ] = Split,

	% Remove whitespaces:
	StrippedFunctionHead = string:strip(FunctionHead),
	%io:format( " StrippedFunctionHead = ~s~n", [StrippedFunctionHead] ),

	case get_function_name( StrippedFunctionHead ) of

		"module_info" ->
			% Skip it:
			build_spec_entries( T, Acc );

		FunctionName ->
			%io:format( "   + function name: ~s~n", [FunctionName] ),
			Arity = get_arity( StrippedFunctionHead ),
			%io:format( "   + function arity: ~B~n", [Arity] ),
			build_spec_entries( T, [ {FunctionName,Arity,Spec++"."} | Acc ] )

	end.



% Returns a list of {FunctionName,Arity,Index} entries.
build_fun_entries( FunctionNames, Text ) ->

	% Builds the RE to gather all function definitions from the text:
	RE = "\n(" ++ lists:flatten( join( _Sep="|", FunctionNames ) )
		++ ")\\([^)]*\\)((?U)(.|\n)*->)",

	%io:format( "RE = ~s~n", [RE] ),

	% We need the index *and* the matched pattern:
	{match,IndexList} = re:run( Text, RE, [ global, {capture,first,index} ] ),
	% IndexList = [ [{2795,19}], [{2976,27}], etc.
	% If list was used instead of index:
	% {match,[["\nget_timestamp() ->"],
	%  ["\nget_textual_timestamp( { {Y,M,D}, {Hour,Minute,Second} } ) ->"], etc.

	% {FunctionHead,Index} pairs:
	% (we do not retain the initial \n):
	Pairs = [ { string:substr(Text,Index+2,Len-1), Index+2 }
			  || [{Index,Len}] <- IndexList ],
	%io:format( "Pairs = ~p~n", [Pairs] ),
	% Pairs = [{"get_timestamp() ->",2797},
	%	 {"get_precise_duration( _FirstTimestamp={A1,A2,A3},\n\t\t\t\t\t
	% _SecondTimestamp={B1,B2,B3} ) ->",5283},
	%	 {"register_as( Pid, Name, local_only ) when is_atom(Name) ->",6250},
	% etc.

	% Now we just have to determine the corresponding function names and arity,
	% as we did for specs:
	FullPairs = [ {get_function_name(FH), get_arity( strip_right_of_sig(FH) ),
				   Index} || {FH,Index} <- Pairs ],

	% We remove any clause after the first, for a {FunName,Arity} combination:
	SortedByArities = lists:keysort( _ArityPos=2, FullPairs ),
	SortedByNames = lists:keysort( _FunNamePos=1, SortedByArities ),

	% Here entries are sorted first by name, then for a name by arity, then for
	% an arity by index, ex:
	%  [      {"append_at_end",2,17043},
	%		  {"checkpoint",1,19801},
	%		  {"compare_versions",2,20079},
	%		  {"deploy_modules",2,18423},
	%		  {"deploy_modules",3,18800},
	%		  {"draw_element",1,20895},
	%		  {"draw_element",2,21280},
	% etc.

	% Thus we can remove all clauses after the first:
	OnlyFirstClauses = filter_clauses( SortedByNames, _Current=none, _Acc=[] ),

	% Re-order by increasing index:
	lists:keysort( _IndexPos=3, OnlyFirstClauses ).


filter_clauses( _Entries=[], _Current, Acc ) ->
	Acc;

filter_clauses( [ {FunName,Arity,_Index} | T ],
				Current={FunName,Arity,_AnyIndex}, Acc ) ->
	% Here head is a non-first clause (name and arity match the current first
	% clause), skip it:
	filter_clauses( T, Current, Acc );

filter_clauses( [H|T], _Current, Acc ) ->
	% Here we change at least arity, it is a new first clause:
	filter_clauses( T, _NewCurrent=H, [H|Acc] ).


% Removes any guard(s) and "->":
strip_right_of_sig( FunctionHead ) ->
	re:replace( FunctionHead, "\\)((?s).*)->", ")", [ {return,list} ] ).


% Returns the function name corresponding to specified signature string.
get_function_name( FunctionHead ) ->
	R = re:replace( FunctionHead, "\\((?s).*", "", [ {return,list} ] ),
	%io:format(  "get_function_name for ~s: ~s.~n", [FunctionHead,R] ),
	R.


% Returns the function arity corresponding to specified signature string.
get_arity( FunctionHead ) ->

	% Extract the parameters spec, first strip lefter part:
	LeftParamStrip = re:replace( FunctionHead, "[a-zA-Z0-9_]+\\(",
								 "", [ {return,list} ] ),

	% Remove the ending ")" (and only that):
	Params = string:substr( LeftParamStrip, _Start=1,
							_Len=length(LeftParamStrip)-1 ),
	%io:format( "Params = ~p~n", [Params] ),

	Arity = compute_first_level_element_count( Params ),
	%io:format( "get_arity for ~s: ~B.~n", [FunctionHead,Arity] ),
	Arity.



% Inserts specifications into source code, and returns the updated code.
insert_spec( Text, FunEntries, SpecEntries ) ->
	insert_spec( Text, FunEntries, SpecEntries, _Offset=0 ).

insert_spec( Text, _FunEntries=[], _SpecEntries=[], _Offset ) ->
	% The two lists should have the same length.
	Text;

insert_spec( Text, _FunEntries, _SpecEntries=[], _Offset ) ->
	% Must be local functions, no spec found for them.
	%throw( {unexpected_function_entry,FunEntries} )
	Text;

insert_spec( Text, _FunEntries=[], SpecEntries, _Offset ) ->

	% This can happen, for example if some functions are defined in the BEAM but
	% not in the .erl source files (ex: WOOPER new operators coming from
	% wooper.hrl):
	%throw( {unexpected_spec_entry,SpecEntries} );
	notify_unexpected_spec( SpecEntries ),
	Text;

insert_spec( Text, [ {FunName,Arity,Index} | T ], SpecEntries, Offset ) ->

	% Jump from index to index, inserting the relevant spec:
	case get_spec_for( FunName, Arity, SpecEntries, _Acc=[] ) of

		not_found ->
			% Must be local functions:
			%io:format( "  (no spec found for ~s/~B).~n", [FunName,Arity] ),
			insert_spec( Text, T, SpecEntries, Offset );

		{Spec,OtherSpecs} ->
			SpecOffset = Index+Offset,

			Left = string:sub_string( Text, _Start=1, SpecOffset-1 ),
			Right = string:sub_string( Text, SpecOffset ),

			ActualSpec = format_spec(Spec) ++ "\n",
			NewText = Left ++ ActualSpec ++ Right,
			NewOffset = Offset + length(ActualSpec),

			insert_spec( NewText, T, OtherSpecs, NewOffset )

	end.



notify_unexpected_spec( [] ) ->
	ok;

notify_unexpected_spec( [ {FunctionName,Arity,_Spec} | T ] ) ->

	case lists:member( {FunctionName,Arity}, get_spec_suppressions() ) orelse
		lists:member( {FunctionName,any}, get_spec_suppressions() ) of

		true ->
			notify( io_lib:format( "~n  (suppressed warning about ~s/~B "
								   "not being found in sources)~n",
								   [FunctionName,Arity] ) ),
			ok;

		false ->
			%io:format( "  ### Warning: function ~s/~B not found in source, "
			%		   "whereas following type specification was found "
			%		   "from BEAM:~n~s.~n", [ FunctionName, Arity, Spec ] ),

			io:format( "~n~n  ### Warning: function ~s/~B not found in source, "
					   "whereas found from BEAM.~n~n",
					  [ FunctionName, Arity ] ),

			notify_unexpected_spec( T )

	end;

notify_unexpected_spec( Other ) ->
	throw( {unexpected_spec_content,Other} ).



% Returns the corresponding spec and the list of remaining ones (order not
% preserved).
get_spec_for( FunName, Arity, _Specs=[ {FunName,Arity,Spec} | T ], Acc ) ->
	{Spec, T++Acc};

get_spec_for( _FunName, _Arity, _Specs=[], _Acc ) ->
	%throw( { spec_not_found_for, {FunName,Arity} } );

	% Actually not finding a spec for a given function is not abnormal, as
	% apparently Dialyzer outputs specs only for *exported* functions.
	not_found;

get_spec_for( FunName, Arity, _Specs=[NonMatchingEntry|T], Acc ) ->
	get_spec_for( FunName, Arity, T, [NonMatchingEntry|Acc] ).


-define( line_max_length, 80 ).

-define( line_header, "  " ).
% Must be length( ?line_header ):
-define( line_offset, 2 ).


% Formats specified spec, for pretty-printing.
% Returns a list of lines.
format_spec( Spec ) when length(Spec) < ?line_max_length ->
	[Spec];

format_spec( Spec ) ->

	% Not smart enough, we would need to cut expressions depending on nesting
	% level:

	% Here the line is too long, split at the arrow:
	%[Left,Right] = re:split( Spec, "->", [ {return,list} ] ),

	%% Lines = case length(Left) + 2 < ?line_max_length of

	%%		true  ->
	%%			[ Left ++ "->" , Right ];

	%%		false ->

	%%			case length(Right) + 2 + ?line_offset < ?line_max_length of

	%%				true ->
	%%					[Left, ?line_header ++ "->" ++ Right ];

	%%				false ->
	%%					[Left, ?line_header ++ "->", ?line_header ++ Right]

	%%			end

	%% end,
	%% join( _Sep="\n", Lines ),
	Spec.




% Returns the full path of the .erl source file corresponding to the specified
% module.
find_source_file_for( ModuleName, Dir ) ->

	% Could be as well the replacement of the extension.

	% (not using "\.erl$" to avoid a problem with emacs syntax highlighting)
	%% ErlPath = filelib:fold_files( Dir, ModuleName ++ "\.erl" ++ [ $$ ], true,
	%%			 fun(File, []) ->
	%%				 File
	%%			 end, []),

	%% case ErlPath of

	%%	[] ->
	%%		throw( {cannot_find_source_file_for,ModuleName,Dir} );

	%%	[Path] ->
	%%		Path;

	%%	MultiplePaths ->
	%%		throw( {multiple_source_files_for,ModuleName,Dir,MultiplePaths} )

	%% end.

	% We assume here this is just a matter of replacing the extension:
	filename:join( Dir, ModuleName ++ ".erl" ).


% Allows to back-up source files before specs are inserted.
perform_any_backup( ErlPath ) ->

	case do_backup() of

		true ->
			Destination = ErlPath ++ ".specbak",
			case file:copy( ErlPath, Destination ) of

				{ok,_BytesCopied} ->
					ok;

				{error,Reason} ->
					throw( {cannot_backup_file,ErlPath,Reason} )

			end;

		false ->
			ok

	end.



% Reads all lines from specified device (file descriptor).
read_all_lines( Device, Acc ) ->

	case io:get_line( Device, "" ) of

		eof  ->
			file:close(Device),
			lists:reverse(Acc);

		Line ->
			read_all_lines( Device, [Line|Acc] )

	end.




% Returns the number of first-level elements in the specified string expression.
% Ex: for expression "3, a, [5,6,{b,1} ]", should return 3.
%
% Simpler to do that way rather than using parse_trans or erl_scan:string/1 then
% erl_parse:abstract/1.
%
compute_first_level_element_count( _ArgString=[] ) ->
	0;

compute_first_level_element_count( ArgString ) ->
	compute_first_level_element_count( ArgString, _BraceLevel=0, _ParenLevel=0,
				_BrackLevel=0, _InitialCount=1 ).


% Expressions must be well-balanced:
compute_first_level_element_count( _ArgString=[], _BraceLevel=0,
		_ParenLevel=0, _BrackLevel=0, Count ) ->
	% Here we parsed correctly the full string:
	Count;

compute_first_level_element_count( [$,|T], _BraceLevel=0, _ParenLevel=0,
								   _BrackLevel=0, Count ) ->
	% Here we found a top-level comma, the only interesting kind:
	compute_first_level_element_count( T, _BraceLevel=0, _ParenLevel=0,
			_BrackLevel=0, Count+1 );

compute_first_level_element_count( [$,|T], BraceLevel, ParenLevel,
								   BrackLevel, Count ) ->
	% Here we found an inner comma, not interesting:
	compute_first_level_element_count( T, BraceLevel, ParenLevel, BrackLevel,
									   Count );

compute_first_level_element_count( [${|T], BraceLevel, ParenLevel,
								   BrackLevel, Count ) ->
	compute_first_level_element_count( T, BraceLevel+1, ParenLevel,
									   BrackLevel, Count );

compute_first_level_element_count( [$}|T], BraceLevel, ParenLevel,
								   BrackLevel, Count ) ->
	compute_first_level_element_count( T, BraceLevel-1, ParenLevel,
									   BrackLevel, Count );

compute_first_level_element_count( [$[|T], BraceLevel, ParenLevel, BrackLevel,
								   Count ) ->
	compute_first_level_element_count( T, BraceLevel, ParenLevel,
									   BrackLevel+1, Count );

compute_first_level_element_count( [$]|T], BraceLevel, ParenLevel, BrackLevel,
								   Count ) ->
	compute_first_level_element_count( T, BraceLevel, ParenLevel, BrackLevel-1,
									   Count );

compute_first_level_element_count( [$(|T], BraceLevel, ParenLevel, BrackLevel,
								   Count ) ->
	compute_first_level_element_count( T, BraceLevel, ParenLevel+1,
									   BrackLevel, Count );

compute_first_level_element_count( [$)|T], BraceLevel, ParenLevel, BrackLevel,
								   Count ) ->
	compute_first_level_element_count( T, BraceLevel, ParenLevel-1, BrackLevel,
									   Count );

compute_first_level_element_count( [_H|T], BraceLevel, ParenLevel, BrackLevel,
								   Count ) ->
	% All other characters:
	compute_first_level_element_count( T, BraceLevel, ParenLevel, BrackLevel,
									   Count ).


notify( Message ) ->

	case is_verbose() of

		true ->
			io:format( Message );

		false ->
			ok

	end.
