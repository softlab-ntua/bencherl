%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2009 Basho Technologies

%% @doc Utility for parsing multipart form bodies.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%% http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(webmachine_multipart).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([get_all_parts/2,stream_parts/2, find_boundary/1]).


% @type incoming_req_body() = binary().
% The request body, in "multipart/form-data" (rfc2388) form,

% @type boundary() = string().
% The multipart boundary, as taken from the containing message's content-type.

% @type fpart() = {fpartname(), {[fparam()],[fheader()]}, fcontent()}.
% A single part of a multipart form.

% @type fpartname() = string().
% The name from the form field of a form part.

% @type fparam() = {binary(), binary()}.
% A key-value parameter from the content-disposition header in a form part.

% @type fheader() = {binary(), binary()}.
% A header name and value supplied within a form part.

% @type fcontent() = binary().
% The body content within a form part.

% @doc Find the multipart boundary for a request.
% @spec find_boundary(wrq:wm_reqdata()) -> boundary()
find_boundary(ReqData) ->
    ContentType = wrq:get_req_header("content-type", ReqData),
    string:substr(ContentType, string:str(ContentType, "boundary=") 
                  + length("boundary=")).

% @doc Turn a multipart form into component parts.
% @spec get_all_parts(incoming_req_body(), boundary()) -> [fpart()]
get_all_parts(Body, Boundary) when is_binary(Body), is_list(Boundary) ->
    StreamStruct = send_streamed_body(Body,1024),
    getparts1(stream_parts(StreamStruct, Boundary), []).

% @doc Similar to get_all_parts/2, but for streamed/chunked bodies.
%   Takes as input the result of wrq:stream_req_body/2, and provides
%   either the atom 'done_parts' when no more parts are available, or
%   a tuple with the next part and a function.  That function will
%   have 0-arity and the same return type as stream_parts/2 itself.
% @spec stream_parts(wm_stream(), boundary()) ->
%                                    'done_parts' | {fpart(), function()}
stream_parts(StreamStruct, Boundary) ->
    stream_form(StreamStruct, "--" ++ Boundary, []).

stream_form(_, _, [<<"----\n">>|_]) -> done_parts;
stream_form(_, _, [<<"--\n">>|_]) -> done_parts;
stream_form({Hunk, Next}, Boundary, []) ->
    stream_form(get_more_data(Next), Boundary, re:split(Hunk, Boundary,[]));
stream_form({Hunk, Next}, Boundary, [<<>>|DQ]) ->
    stream_form({Hunk, Next}, Boundary, DQ);
stream_form({Hunk, Next}, Boundary, [H|[T1|T2]]) ->
    {make_part(H), fun() ->
                    stream_form({Hunk, Next}, Boundary, [T1|T2]) end};
stream_form({Hunk, really_done}, Boundary, DQ) ->
    DQBin = iolist_to_binary(DQ),
    FullHunk = <<DQBin/binary, Hunk/binary>>,
    stream_parts(re:split(FullHunk, Boundary,[]));
stream_form({Hunk, Next}, Boundary, [Single]) ->
    FullHunk = <<Single/binary, Hunk/binary>>,
    stream_form(get_more_data(Next), Boundary, re:split(FullHunk, Boundary,[])).

stream_parts([]) -> done_parts;
% browsers are fun, and terminate posts slightly differently from each other:
stream_parts([<<"----\n">>]) -> done_parts;
stream_parts([<<"--\n">>]) -> done_parts;
stream_parts([<<"----\r\n">>]) -> done_parts;
stream_parts([<<"--\r\n">>]) -> done_parts;
stream_parts([<<"--\r\n--\n">>]) -> done_parts;
stream_parts([<<"--\r\n--\r\n">>]) -> done_parts;
stream_parts([H|T]) -> {make_part(H), fun() -> stream_parts(T) end}.

get_more_data(done) -> {<<"--\n">>, really_done};
get_more_data(Fun) -> Fun().
   
make_part(PartData) ->
    [HeadData, Body] = re:split(PartData, "\\r\\n\\r\\n", [{parts,2}]),
    HeadList = [list_to_binary(X) ||
                   X <- string:tokens(binary_to_list(HeadData), "\r\n")],
    {Name, Params, Headers} = make_headers(HeadList),
    {Name, {Params,Headers}, Body}.

make_headers(X) -> 
    make_headers(X, name_undefined, params_undefined, []).
make_headers([], Name, Params, Headers) -> {Name, Params, Headers};
make_headers([<<>>|HL], Name, Params, Headers) ->
    make_headers(HL, Name, Params, Headers);
make_headers(
  [<<"Content-Disposition: form-data; ", Names/binary>>|HL],
  _, _, Headers) ->
    {Name, Params} = extract_names(Names),
    make_headers(HL, Name, Params, Headers);
make_headers([H|HL], Name, Params, Headers) ->
    make_headers(HL, Name, Params, [cheap_parse_header(H)|Headers]).

extract_names(NamesString) ->
    Params = [{K, V} ||
              {K, [<<>>, V, <<>>]} <- [{K0, re:split(V0,"\"",[])} ||
                          [K0, V0] <- [re:split(N, "=", [{parts, 2}]) ||
                                 N <- re:split(NamesString, "; ", [])]]],
    Name = hd([binary_to_list(V) || {<<"name">>,V} <- Params]),
    {Name, Params}.

cheap_parse_header(HeadBin) ->
    [K,V] = re:split(HeadBin, ": ", [{parts,2}]),
    {K,V}.

getparts1(done_parts, Acc) ->
    lists:reverse(Acc);
getparts1({Part, Streamer}, Acc) ->
    getparts1(Streamer(), [Part|Acc]).

send_streamed_body(Body, Max) ->
    HunkLen=8*Max,
    case Body of        
        <<A:HunkLen,Rest/binary>> ->
            {<<A:HunkLen>>, fun() -> send_streamed_body(Rest,Max) end};
        _ ->
            {Body, done}
    end.

