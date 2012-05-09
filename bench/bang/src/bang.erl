-module(bang).

-export([bench_args/0, run/3]).

bench_args() ->
	[[S,M] || S <- [1000], M <- [1000]].

run([S,M|_], _, _) ->
	Parent = self(),
	Done   = make_ref(),
	Bang   = {make_ref(),make_ref(),make_ref(),make_ref(),make_ref()},
	Rec    = spawn_opt(fun () -> rec(Bang, S*M), Parent ! Done end, [link]),
	lists:foreach(fun(_) ->
		spawn_link(fun () -> send(Rec, Bang, M) end)
	end, lists:seq(1, S)),
	receive Done -> ok end,
	ok.

send(_T, _M, 0) -> ok;
send(T, M, N)   -> T ! M, send(T, M, N-1).

rec(_M, 0) -> ok;
rec(M, N)  -> receive M -> rec(M, N-1) end.

