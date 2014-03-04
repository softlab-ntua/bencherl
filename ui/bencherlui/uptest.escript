#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname uptest
main([String]) ->
    case net_adm:ping(list_to_atom(String)) of
        pong -> halt(0);
        pang -> halt(1)
    end.
