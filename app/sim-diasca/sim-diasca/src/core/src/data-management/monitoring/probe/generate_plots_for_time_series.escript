#!/usr/bin/env escript


% Allows to generate the plots (PNG files, usually for probe reports)
% corresponding to all the time series (*.dat and *.p) files found in the
% current directory.
%
%-module(generate_plots_for_time_series).

%-export([ run/0 ]).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


main( [ Dir ] ) when is_list( Dir ) ->
	run( Dir ).



run( Dir ) ->

	Dir = "",
	CoreCount = get_core_count(),

	Filenames = select_data_files( Dir ),

	io:format( "~n~nGenerating over ~B cores the plots for the ~B time series "
			   "found in directory '~s': ~s~n",
			   [ CoreCount, length(Filenames), Dir,
				 string_list_to_string(Filenames) ] ),

	% Ensures there is up to one idle worker for maximum loading:
	MaxWorkerCount = CoreCount + 1,

	manage_workers( _DataFilenames=Filenames, _CurrentWorkers=[],
					_GeneratedFilenames=[], _ReportedErrors=[],
					MaxWorkerCount ),

	io:format( "End of generation script.~n" ).



% Returns a list of the filenames corresponding to time series.
select_data_files( DirectoryName ) ->
	{ RegularFiles, _Directories, _OtherFiles, _Devices } = list_dir_elements(
			DirectoryName ),
	filter_by_extension( RegularFiles, ".dat" ).



manage_workers( _DataFilenames=[], _CurrentWorkers=[], _GeneratedFilenames=[],
				_ReportedErrors=[], _MaxWorkerCount ) ->
	io:format( "Nothing was to be generated.~n" );

manage_workers( _DataFilenames=[], _CurrentWorkers=[], GeneratedFilenames,
				_ReportedErrors=[], _MaxWorkerCount ) ->

	io:format( "All ~B reports were successfully generated: ~s~n",
			  [ length( GeneratedFilenames ),
			   string_list_to_string( GeneratedFilenames ) ] );

manage_workers( _DataFilenames=[], _CurrentWorkers=[], GeneratedFilenames,
				ReportedErrors, _MaxWorkerCount ) ->

	ErrorStrings = [ io_lib:format( "for ~s: ~s", [ File, Error ] )
					|| { File, Error } <- ReportedErrors ],

	GenLen = length( GeneratedFilenames ),
	ErrLen = length( ReportedErrors ),

	io:format( "Out of ~B reports, ~B were successfully generated:~s~n"
			   "Whereas ~B generations failed:~s~n",
			  [ GenLen + ErrLen, GenLen,
			   string_list_to_string( GeneratedFilenames ), ErrLen,
			   string_list_to_string( ErrorStrings ) ] );

manage_workers( DataFilenames, CurrentWorkers, GeneratedFilenames,
				ReportedErrors, MaxWorkerCount ) ->

	{ RemainingWorkers, NewGeneratedFilenames, NewReportedErrors } = receive

		{ work_done, Pid, PNGFilename } ->
			%io:format( "Worker ~w finished.~n", [ Pid ] ),
			{ lists:delete( Pid, CurrentWorkers ),
			 [ PNGFilename | GeneratedFilenames ], ReportedErrors };

		{ work_failed, Pid, Error }->
			%io:format( "Worker ~w failed: ~s.~n", [ Pid, Reason ] ),
			{ lists:delete( Pid, CurrentWorkers ), GeneratedFilenames,
			 [ Error | ReportedErrors ] }

	after 100 ->

		% Needed to bootstrapt workers:
		{ CurrentWorkers, GeneratedFilenames, ReportedErrors }

	end,

	{NewDataFilenames,NewWorkers} = update_workers( DataFilenames,
										RemainingWorkers, MaxWorkerCount ),

	manage_workers( NewDataFilenames, NewWorkers, NewGeneratedFilenames,
					NewReportedErrors, MaxWorkerCount ).



update_workers( _DataFilenames=[], Workers, _MaxWorkerCount ) ->
	% Last workers are still working, just wait (do nothing):
	{ [], Workers };


update_workers( _DataFilenames=[ Filename | T ], Workers, MaxWorkerCount )
  when length( Workers ) < MaxWorkerCount ->

	% There is still work to be done, and room for one more worker here:

	DispatcherPid = self(),

	F = fun() ->
				manage_plot( Filename, DispatcherPid )
		end,

	NewWorkerPid = spawn_link( F ),
	{ T, [ NewWorkerPid | Workers ] }.





% The main function of workers.

manage_plot( DataFilename, DispatcherPid ) ->

	CommandFilename = replace_extension( DataFilename, ".dat", ".p" ),

	case is_existing_file( CommandFilename ) of

		true ->
			%io:format( "Command file found.~n" ),
			case generate_report( DataFilename, CommandFilename ) of

				{ success, TargetFilename } ->
					DispatcherPid ! { work_done, self(), TargetFilename };

				{ failure, Reason } ->
					DispatcherPid ! { work_failed, self(),
									  { DataFilename, Reason } }

			end;

		false ->
			Message = io_lib:format( "command file '~s' not found",
									 [ CommandFilename ] ),

			DispatcherPid ! { work_failed, self(), { DataFilename, Message } }

	end.



% Largely inspired from class_Probe:generate_report/2.
generate_report( DataFilename, CommandFilename ) ->

	% Gnuplot might issue non-serious warnings. Generates a PNG:
	Message = case os:cmd( "gnuplot " ++ CommandFilename ) of

		[] ->
			[];

		M ->
			io:format( "Warning: report generation for ~s resulted in "
					   "following output: ~s.", [ DataFilename, M ] ),
			M

	end,

	% Hack for .png:
	TargetFilename = CommandFilename ++ "ng",

	case is_existing_file( TargetFilename ) of

		true ->
			{ success, TargetFilename };

		false ->

			case Message of

				[] ->
					{ failure, io_lib:format( "generation failed for ~s",
											  [ DataFilename ] ) };

				_ ->
					{ failure, io_lib:format( "generation failed for ~s: ~s",
											  [ DataFilename, Message ] ) }

			end

	end.





% Duplicated code section.



% From file_utils.erl:


% Returns a tuple made of a four lists describing the file elements found in
% specified directory: { RegularFiles, Directories, OtherFiles, Devices }.
%
list_dir_elements(Dirname) ->

	%io:format( "list_dir_elements for '~s'.~n", [ Dirname ] ),

	{ ok, LocalDirElements } = file:list_dir( Dirname ),
	classify_dir_elements( Dirname, LocalDirElements, [], [], [], [] ).


% Returns the type of the specified file entry, in:
% device | directory | regular | other.
%
get_type_of( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, FileInfo } ->
			#file_info{ type=FileType } = FileInfo,
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, EntryName } );

		{ error, enoent } ->
			throw( { non_existing_entry, EntryName } )

	end.



% Returns a tuple containing four lists corresponding to the sorting of all
% file elements: { Directories, RegularFiles, Devices, OtherFiles }.
%
classify_dir_elements( _Dirname, [],
					   Devices, Directories, RegularFiles, OtherFiles ) ->
	% Note the reordering:
	{ RegularFiles, Directories, OtherFiles, Devices };

classify_dir_elements( Dirname, [ H | T ], Devices, Directories, RegularFiles,
					   OtherFiles ) ->

	 case get_type_of( filename:join( Dirname, H ) ) of

		device ->
			classify_dir_elements( Dirname, T, [ H | Devices ], Directories,
								   RegularFiles, OtherFiles ) ;

		directory ->
			classify_dir_elements( Dirname, T, Devices, [ H | Directories ],
								   RegularFiles, OtherFiles ) ;

		regular ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   [ H | RegularFiles ], OtherFiles ) ;

		other ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   RegularFiles, [ H | OtherFiles ] )

	end.



% Returns the current directory, as a plain string.
%
% Throws an exception on failure.
%
% (verbatim from file_utils)
%
get_current_directory() ->

	case file:get_cwd() of

		{ ok, Dir} ->
			Dir;

		{ error, Reaso n} ->
			throw( { failed_to_determine_current_directory, Reason } )

	end.



% Returns a list containing all elements of Filenames list whose extension is
% the specified one.
%
filter_by_extension( Filenames, Extension ) ->
	filter_by_extension( Filenames, Extension, [] ).


filter_by_extension( [], _Extension, Acc ) ->
	Acc ;

filter_by_extension( [ H | T ], Extension, Acc ) ->
	case filename:extension( H ) of

		Extension ->
			filter_by_extension( T, Extension, [ H | Acc ] ) ;

		_Other ->
			filter_by_extension( T, Extension, Acc )

	end.


% Returns a new filename whose extension has been updated.
%
% Ex: replace_extension( "/home/jack/rosie.ttf", ".ttf", ".wav" ) should return
% "/home/jack/rosie.wav".
%
replace_extension( Filename, SourceExtension, TargetExtension ) ->
	Index = string:rstr( Filename, SourceExtension ),
	string:substr( Filename, 1, Index-1 ) ++ TargetExtension.


% Returns whether the specified entry exists and is a regular file.
%
% Returns true or false, and cannot trigger an exception.
%
is_existing_file( EntryName ) ->
	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.


% Tells whether specified file entry exists, regardless of its type.
%
exists( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason } ->
			false

	end.




% From system_utils:


% Returns the number of cores available on the local host.
%
% Throws an exception on failure.
%
get_core_count() ->

	String = remove_ending_carriage_return(
				os:cmd( "cat /proc/cpuinfo | grep -c processor" ) ),

	try
		string_to_integer( String )
	catch

		{ integer_conversion_failed, String } ->
			throw( { could_not_determine_core_count, String } )

	end.



% From text_utils.erl:


% Returns a string which pretty-prints specified list of strings, with bullets.
%
string_list_to_string( ListOfStrings ) ->
	io_lib:format( "~n~s", [ string_list_to_string(
								 ListOfStrings, _Acc=[], _Bullet=" + " ) ] ).

string_list_to_string( _ListOfStrings=[], Acc, _Bullet ) ->
	 Acc;

string_list_to_string( _ListOfStrings=[ H | T ], Acc, Bullet )
  when is_list( H ) ->
	string_list_to_string( T, Acc ++ Bullet ++ io_lib:format( "~s~n", [ H ] ),
						   Bullet ).


% Removes the ending "\n" character(s) of specified string.
%
remove_ending_carriage_return( String ) when is_list( String ) ->
	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).



% Returns an integer which corresponds to the specified text.
%
% Throws an exception if the conversion failed.
%
string_to_integer( String ) ->

	try list_to_integer( String ) of

		I ->
			I

	catch

		error:badarg ->
			throw( { integer_conversion_failed, String } )

	end.
