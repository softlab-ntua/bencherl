%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2012 VMware, Inc.  All rights reserved.
%%

-module(rabbit_jsonrpc).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    RpcContext = case application:get_env(?MODULE, context) of
                     undefined -> "rpc";
                     {ok, V} -> V
                 end,
    rabbit_mochiweb:register_context_handler(
        jsonrpc,
        RpcContext,
        fun(_, Req) ->
            case rfc4627_jsonrpc_mochiweb:handle("/" ++ RpcContext, Req) of
                no_match ->
                    Req:not_found();
                {ok, Response} ->
                    Req:respond(Response)
            end
        end, "JSON-RPC: RPC endpoint"),
    {ok, spawn(fun loop/0)}.

stop(_State) ->
    rabbit_mochiweb:unregister_context(jsonrpc),
    ok.

loop() ->
  receive
    _ -> loop()
  end.
