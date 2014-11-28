-module(rhesus).
-export([chaos_on/0, chaos_on/1, extract_pids/2]).

%%===============================================================================
%% CHAOS GENERATION LOGIC
%%===============================================================================
chaos_on() ->
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1,A2,A3),
    Timer = (random:uniform(3595) + 5) * 1000,
    io:format("A random process will be killed every ~p seconds.~n", [Timer div 1000]),
    chaos_on(Timer, undefined).

chaos_on(Timer) ->
    chaos_on(Timer, undefined).

chaos_on(Timer, Router_Sup_Pid) ->
    case Router_Sup_Pid of
	undefined ->
	    rhesus_gets_nervous(Timer);
	Pid ->
	    case is_process_alive(Pid) of
		true ->
		    rhesus_attack(Timer, Pid);
		false ->
		    rhesus_gets_nervous(Timer)
	    end
    end.

rhesus_gets_nervous(Timer) ->
    case find_router_sup_pid() of
	not_found ->
	    io:format("Router supervisor process not found on this node.~n");
	R_Sup_Pid ->
	    rhesus_attack(Timer, R_Sup_Pid)
    end.

rhesus_attack(Timer, Router_Sup_Pid)->
    timer:sleep(Timer),
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1,A2,A3),
    case random:uniform(4) of
	1 ->
	    Router_Sup_Pid ! rhesus_solves_conflict_router,
	    chaos_on(Timer, Router_Sup_Pid);
	_Other ->
	    Router_Sup_Pid ! rhesus_solves_conflict_server,
	    chaos_on(Timer, Router_Sup_Pid)
    end.

find_router_sup_pid() ->
    find_router_sup_pid(erlang:processes()).

find_router_sup_pid(List) ->
    case List of
	[] ->
	    not_found;
	[H|T] ->
	    {Name,Tuple} = hd(erlang:process_info(H)),
	    case Name of
		current_function ->
		    {_,F,_} = Tuple,
		    case F of
			router_supervisor ->
			    H;
			_Other ->
			    find_router_sup_pid(T)
		    end;
		_Any_other ->
		    find_router_sup_pid(T)
	    end
    end.

extract_pids(List_Pids, List_Routers) ->
    case List_Routers of
	[] ->
	    List_Pids;
	[H|T] ->
	    {_Name, Pid} = H,
	    extract_pids([Pid|List_Pids],T)
    end.

