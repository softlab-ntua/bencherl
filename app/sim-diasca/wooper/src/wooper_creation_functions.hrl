% Modular WOOPER header gathering the primitives to manage the creation of
% instances.



% The creation of an instance of a WOOPER class can be:
%
% - either non-blocking or synchronous (and, if synchronous, with or without a
% time-out)
%
% - either not linked or linked to the current process
%
% - either local (on the current node) or remote (on another node)



% Following construction variations are always declared, for a constructor
% expected to take N base parameters:
% new/N, new_link/N, synchronous_new/N, synchronous_new_link/N

% If use_synchronous_timed_new is defined, WOOPER adds:
% synchronous_timed_new/N, synchronous_timed_new_link/N

% If use_remote_new is defined, WOOPER adds:
% remote_new/N+1, remote_new_link/N+1, remote_synchronous_new/N+1,
% remote_synchronous_new_link/N+1, remote_synchronisable_new_link/N+1.

% Finally, if use_synchronous_timed_new *and* use_remote_new are defined,
% WOOPER adds these timed operations:
% remote_synchronous_timed_new/N+1, remote_synchronous_timed_new_link/N+1.

% Therefore for a template for a full-blown declaration would be:
% (in the next block, just search and replace with your text editor A with N,
% and B with N+1)


% Declaring all variations of WOOPER standard life-cycle operations:
%
%-define( wooper_construct_export, new/A, new_link/A,
%	synchronous_new/A, synchronous_new_link/A,
%	synchronous_timed_new/A, synchronous_timed_new_link/A,
%	remote_new/B, remote_new_link/B, remote_synchronous_new/B,
%	remote_synchronous_new_link/B, remote_synchronisable_new_link/B,
%   remote_synchronous_timed_new/B, remote_synchronous_timed_new_link/B,
%   construct/B, destruct/1 ).


% Note: destruct/1 can be removed from the export list above if no specific
% destructor is to be defined.



% There are construction operators that just take the construction parameters,
% ?wooper_construct_parameters (like new/N), and other operators that take
% an additional parameter, the target node (like remote_new/N+1).
%
% As wooper_construct_parameters can be void (i.e. declared as
% '-define(wooper_construct_parameters,).'), the declaration 'new(
% ?wooper_construct_parameters )' would be correct, whereas 'remote_new(
% Node, ?wooper_construct_parameters )' would result in the incorrect syntax
% 'remote_new( Node, )'.
%
% A solution could be, depending on wooper_construct_parameters being void or
% not, to define either 'remote_new( Node )' (if void) or 'remote_new(
% Node,?wooper_construct_parameters )' (if not void).
%
% However the Erlang macros do not seem to support tests based on their value
% (we can only test whether they are defined or not), thus the following
% convention has been used:

% wooper_construct_parameters should be defined if and only if there is at least
% one parameter to be declared.
%
% Therefore, in the case of a constructor taking two parameters, X and Y, we
% will have:
%
% -define( wooper_construct_parameters, X, Y ).
% ...
% construct( State, ?wooper_construct_parameters ) ->
% ...
%
% whereas in the case of a constructor taking no parameter, we will have simply:
%
% ...
% construct( State ) ->
% ...
%

% i.e. there will be in this case no: '-define(wooper_construct_parameters,)'.


% No specification can be provided for new operators, due to their
% class-specific arities.



% Uncomment to activate synchronous, timed constructions:
%
% (yes, we want to enable them)
%
-define(use_synchronous_timed_new,).



% Uncomment to activate remote new constructions:
%
% (yes, we want to enable them)
%
-define(use_remote_new,).




% First case: wooper_construct_parameters is defined:

-ifdef(wooper_construct_parameters).



% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
%
new( ?wooper_construct_parameters ) ->

	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),

	spawn( fun() -> wooper_construct_and_run( [ ?wooper_construct_parameters ] )
						end ).



% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
%
% Returns the PID of the newly created and linked instance.
%
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
%
new_link( ?wooper_construct_parameters ) ->

	spawn_link( fun() -> wooper_construct_and_run(
					  [ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
%
synchronous_new( ?wooper_construct_parameters ) ->

	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
%
synchronous_new_link( ?wooper_construct_parameters ) ->

	%io:format( "synchronous_new_link for ~s with parameters:~n~p.~n",
	%		  [ ?MODULE, [ ?wooper_construct_parameters ] ] ),

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			%io:format( "synchronous_new_link: spawned ~w.~n", [SpawnedPid] ),
			SpawnedPid

	end.






-ifdef(use_synchronous_timed_new).




% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
%
synchronous_timed_new( ?wooper_construct_parameters ) ->

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_time_out, ?MODULE } )

	end.



% Spawns a new instance for this class, and links it to the current process,
% using specified parameters to construct it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new_link will return only when the
% created process reports it is up and running, or when a time-out occurs.
%
synchronous_timed_new_link( ?wooper_construct_parameters ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_linked_time_out, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new




% If use_remote_new is defined, following construction variations will be
% automatically defined (and thus class implementor will have to declare them):
%
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - remote_synchronisable_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then we have remote_new/N+1.



-ifdef(use_remote_new).


% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
%
remote_new( Node, ?wooper_construct_parameters ) ->

	spawn( Node, fun() -> wooper_construct_and_run(
						  [ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process, using specified parameters to construct it.
%
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
%
remote_new_link( Node, ?wooper_construct_parameters ) ->

	spawn_link( Node, fun() -> wooper_construct_and_run(
		[ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new will return only when the
% created process reports it is up and running.
%
remote_synchronous_new( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_new operator: "
	% "spawning ~w:wooper_construct_and_run_synchronous "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new_link will return only when the
% created process reports it is up and running.
%
remote_synchronous_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_new_link operator: "
	% "spawning ~w:wooper_construct_and_run_synchronous "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
												CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous (the PID is directly returned), however a
% {spawn_successful,SpawnedPid} message will be received once (if ever) the
% instance is up and running. This allows to perform the actual instance
% creations in parallel, by waiting bulks of creations.
%
remote_synchronisable_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronisable_new_link operator: "
	% "spawning ~w:wooper_construct_and_run_synchronous "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),
	%timer:sleep(200),

	CreatorPid = self(),

	spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
												CreatorPid ) end ).



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new will return
% only when the created process reports it is up and running, or when
% a time-out occurs.
%
remote_synchronous_timed_new( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_timed_new operator: "
	% "spawning ~w:wooper_construct_and_run_synchronous "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_time_out, Node, ?MODULE } )

	end.



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new_link will return only
% when the created process reports it is up and running, or when a time-out
% occurs.
%
remote_synchronous_timed_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_timed_new_link operator: "
	%		  "spawning ~w:wooper_construct_and_run_synchronous "
	%		  "with parameters ~w on node ~w from node ~w.~n",
	%		  [ ?MODULE, [ ?wooper_construct_parameters ] , Node, node() ] ),

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
											CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			%io:format( "remote_synchronous_timed_new_link: returning ~w.~n",
			%		  [ SpawnedPid ] ),
			SpawnedPid

	after ?synchronous_time_out ->

		io:format( "(remote_synchronous_timed_new_link: throwing time-out "
				   "on node ~p for module ~p after ~p milliseconds)~n",
				   [ Node, ?MODULE, ?synchronous_time_out ] ),

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new







-else. % -ifdef(wooper_construct_parameters).



% Second case: wooper_construct_parameters is *not* defined:
% No argument, thus specs can be defined.


% Spawns a new instance for this class.
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
%
-spec new() -> pid().
new() ->

	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [ ?MODULE ] ),

	spawn( fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class and links it to the current process.
%
% Returns the PID of the newly created and linked instance.
%
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec new_link() -> pid().
new_link() ->

	spawn_link( fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
%
-spec synchronous_new() -> pid().
synchronous_new() ->

	%io:format("synchronous_new operator: spawning ~w "
	%	"with no parameter.~n", [ ?MODULE ] ),

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class and links it to the current process.
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
%
-spec synchronous_new_link() -> pid().
synchronous_new_link() ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



-ifdef(use_synchronous_timed_new).




% Spawns a new instance for this class.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
%
-spec synchronous_timed_new() -> pid().
synchronous_timed_new() ->

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
									   [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_time_out, ?MODULE } )

	end.



% Spawns a new instance for this class, and links it to the current process.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
%
-spec synchronous_timed_new_link() -> pid().
synchronous_timed_new_link() ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		 [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_linked_time_out, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new



% If use_remote_new is defined, following construction variations will be
% automatically defined (thus class implementor will have to declare them):
%
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then having remote_new/N+1.




-ifdef(use_remote_new).



% Spawns a new instance for this class on specified interconnected node.
%
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec remote_new( net_utils:node_name() ) -> pid().
remote_new( Node ) ->
	spawn( Node, fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process.
%
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec remote_new_link( net_utils:node_name() ) -> pid().
remote_new_link( Node ) ->
	spawn_link( Node, fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class on specified interconnected node.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new will return only when the
% created process reports it is up and running.
%
-spec remote_synchronous_new( net_utils:node_name() ) -> pid().
remote_synchronous_new( Node ) ->

	%io:format("synchronous_new operator: spawning ~w "
	%	"with no parameter.~n", [ ?MODULE ]),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new_link will return only when the
% created process reports it is up and running.
%
-spec remote_synchronous_new_link( net_utils:node_name() ) -> pid().
remote_synchronous_new_link( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous (the PID is directly returned), however a {
% spawn_successful, SpawnedPid } message will be received once (if ever) the
% instance is up and running.
%
% This allows to perform the actual instance creations in parallel, by waiting
% bulks of creations.
%
remote_synchronisable_new_link( Node ) ->

	%io:format( "remote_synchronisable_new_link operator: "
	% "spawning ~w:wooper_construct_and_run_synchronous "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),
	%timer:sleep(200),

	CreatorPid = self(),

	spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ).



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new will return only when
% the created process reports it is up and running, or when a time-out occurs.
%
-spec remote_synchronous_timed_new( net_utils:node_name() ) -> pid().
remote_synchronous_timed_new( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_time_out, Node, ?MODULE } )

	end.



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new_link will return only
% when the created process reports it is up and running, or when a time-out
% occurs.
%
-spec remote_synchronous_timed_new_link( net_utils:node_name() ) -> pid().
remote_synchronous_timed_new_link( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new


-endif. % -ifdef(wooper_construct_parameters).





% Extensive testings in this mode.

% Indirection level to allow constructors to be chained.
%
% Allows to obtain the virtual table from the instance, not from its parent.
%
-spec wooper_construct_and_run( [ method_argument() ] ) -> no_return().
wooper_construct_and_run( ConstructionParameters ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",
	%   [ ConstructionParameters, length( ConstructionParameters ) ] ),

	wooper:construct_and_run( _Classname=?MODULE, ConstructionParameters ).



% Indirection level to allow constructors to be chained.
%
% Allows to obtain the virtual table from the instance, not from its parent.
%
-spec wooper_construct_and_run_synchronous( [ method_argument() ], pid() ) ->
												  no_return().
wooper_construct_and_run_synchronous( ConstructionParameters, SpawnerPid ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",
	%   [ ConstructionParameters, length( ConstructionParameters ) ] ),

	wooper:construct_and_run_synchronous( _Classname=?MODULE,
										 ConstructionParameters, SpawnerPid ).
