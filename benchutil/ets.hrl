-define(READ_CONCURRENCY_VERSION, "R14B").
-define(WRITE_CONCURRENCY_VERSION, "R13B02").

supports_ets_concurrency(Version) -> 
	FunErlangVersionGreaterEqual =
		fun([X1|_] = X, [Y1|_] = Y) ->
			case {X1, Y1} of
				{A, A} -> X >= Y;
				{$R, _} -> false;
				{_, $R} -> true;
				{_, _} -> X >= Y % for versions > 19
				end
		end,
	ReadConc = FunErlangVersionGreaterEqual(Version, ?READ_CONCURRENCY_VERSION),
	WriteConc = FunErlangVersionGreaterEqual(Version, ?WRITE_CONCURRENCY_VERSION),
	{ReadConc, WriteConc}.

%supports_ets_read_concurrency(Version) ->
%	{X, _} = supports_ets_concurrency(Version),
%	X.
%supports_ets_write_concurrency(Version) ->
%	{_, X} = supports_ets_concurrency(Version),
%	X.
