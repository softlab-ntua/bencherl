%% DEbench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_config.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_config).

-export([load/1,
         set/2,
         get/1, get/2]).

-include("de_bench.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

load(Files) ->
    TermsList =
        [ case file:consult(File) of
              {ok, Terms} ->
                  Terms;
              {error, Reason} ->
                  ?FAIL_MSG("Failed to parse config file ~s: ~p\n", [File, Reason])
          end || File <- Files ],
    load_config(lists:append(TermsList)).

set(Key, Value) ->
    ok = application:set_env(de_bench, Key, Value).

get(Key) ->
    case application:get_env(de_bench, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            erlang:error("Missing configuration key", [Key])
    end.

get(Key, Default) ->
    case application:get_env(de_bench, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

load_config([]) ->
    ok;
load_config([{Key, Value} | Rest]) ->
    ?MODULE:set(Key, Value),
    load_config(Rest);
load_config([ Other | Rest]) ->
    ?WARN("Ignoring non-tuple config value: ~p\n", [Other]),
    load_config(Rest).

