%%% -*- erlang -*-
%%%
%%% This file is part of nplib released under the MIT license
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2015 Benoit Chesneau
%%%
%%% @doc HTTP 1.1 decoding in pure Erlang
%%% This parser is able to parse HTTP responses and requests in a
%%% streaming fashion. If not set it will be autodetect the type of
%%% binary parsed, if it's a request or a response.
-module(nplib_http).

-export([decode/3]).


-type http_version() :: {integer(), integer()}.
-type status() :: integer().
-type http_reason() :: binary().
-type http_method() :: binary().
-type uri() :: binary().

-type http_result() ::
    {ok, {http_response, http_version(), status(), http_reason()}, binary()}
    | {ok, {http_request, http_version(), http_method(), uri()}, binary()}
    | {ok, http_eoh, binary()}
    | {more, term()}
    | {error, term()}.

-type http_option() :: request | response | auto
    | {max_empty_lines, integer()}
    | {max_line_length, integer()}.

-type chunk() :: binary().
-type trailers() :: list().

-type chunked_result() :: {ok, {http_chunk, chunk()}, binary()}
                         | {ok, {http_chunk_done, trailers()}, binary()}
                         | {ok, http_chunked_done, binary()}
                         | {more, term()}
                         | {error, term()}.

-type http_options() :: [http_option()].

-type decode_type() :: http | httph | http_chunked.

-record(hstate, {type=auto,
                 max_line_length=4096,
                 max_empty_lines=0}).


%% @doc Execute the parser with the current buffer.
%% Types of parsing are:
%% <ul>
%%   <li>`http': to decode the first line of the request</li>
%%   <li>`httph': to decode an header</li>
%%   <li>`http_chunked': to decode a chunked body</li>
%% </ul>
%%
%% Available options:
%% <ul>
%%   <li>`auto' : autodetect if the binary parsed is a response or a
%%   request (default).</li>
%%   <li>`response': set the parser to parse a response</li>
%%   <li>`request': set the parser to parse a request (server)</li>
%%   <li>`{max_line_lenght, Max}': set the maximum size of a line parsed
%%   before we give up.</li>
%%   <li>`{max_lines_empty, Max}': the maximum number of empty line we
%%   accept before the first line happen</li>
%% </ul>
-spec decode(decode_type(), binary(), http_options()) ->
    http_result() | chunked_result().
decode(Type, Bin, Options) ->
    Native = proplists:get_value(native, Options, false),
    case {Type, Native} of
        {http, false} ->
            St = parse_options(Options, #hstate{}),
            parse_first_line(Bin, St, 0);
        {http, _} ->
            St = parse_options(Options, #hstate{}),
            parse_native_first_line(Bin, St, St#hstate.max_empty_lines);
        {httph, false} -> parse_header(Bin);
        {http, _} -> parse_native_header(Bin);
        {http_chunked, _} -> decode_chunked_body(Bin)
    end.


%% Empty lines must be using \r\n.
parse_first_line(<< $\n, _/binary >>, _St, _) ->
    {error, badarg};
%% We limit the length of the first-line to MaxLength to avoid endlessly
%% reading from the socket and eventually crashing.
parse_first_line(Buffer, St=#hstate{type=Type,
                                    max_line_length=MaxLength,
                                    max_empty_lines=MaxEmpty}, Empty) ->
    case match_eol(Buffer, 0) of
        nomatch when byte_size(Buffer) > MaxLength ->
            {error, line_too_long};
        nomatch ->
            {more, undefined};
        1 when Empty =:= MaxEmpty ->
            {error, invalid_http_message};
        1 ->
            << _:16, Rest/binary >> = Buffer,
            parse_first_line(Rest, St, Empty + 1);
        _ when Type =:= auto ->
            case parse_request_line(Buffer) of
                {error, invalid_http_message} ->
                    case parse_response_line(Buffer) of
                        {error, invalid_http_message} = Error ->
                            Error;
                        OK ->
                            OK
                    end;
                OK ->
                    OK
            end;
        _ when Type =:= response ->
            parse_response_line(Buffer);
        _ when Type =:= request ->
            parse_request_line(Buffer)
    end.

match_eol(<< $\n, _/bits >>, N) ->
    N;
match_eol(<< _, Rest/bits >>, N) ->
    match_eol(Rest, N + 1);
match_eol(_, _) ->
    nomatch.

%% @doc parse status
parse_response_line(Buf) ->
    case binary:split(Buf, <<"\r\n">>) of
        [Line, Rest] ->
            parse_response_version(Line, Rest);
        _ ->
            {error, invalid_http_message}
    end.


parse_response_version(<< "HTTP/", High, ".", Low, $\s, Rest/binary >>, Buf)
        when High >= $0, High =< $9, Low >= $0, Low =< $9 ->
    Version = { High -$0, Low - $0},
    parse_status(Rest, Buf, Version, <<>>);
parse_response_version(_, _) ->
     {error, invalid_http_message}.

parse_status(<< C, Rest/bits >>, Buf, Version, Acc) ->
    case C of
        $\r ->  {error, invalid_http_message};
        $\s -> parse_reason(Rest, Buf, Version, Acc);
        _ -> parse_status(Rest, Buf, Version, << Acc/binary, C >>)
    end.

parse_reason(Reason, Buf, Version, StatusCode) ->
    StatusInt = list_to_integer(binary_to_list(StatusCode)),
    {ok, {http_response, Version, StatusInt, Reason}, Buf}.


parse_request_line(Buf) ->
    parse_method(Buf, <<>>).

parse_method(<< C, Rest/bits >>, Acc) ->
    case C of
        $\r ->  {error, invalid_http_message};
        $\s -> parse_uri(Rest, Acc);
        _ -> parse_method(Rest, << Acc/binary, C >>)
    end.

parse_uri(<< $\r, _/bits >>,  _) ->
    {error, invalid_http_message};
parse_uri(<< "* ", Rest/bits >>, Method) ->
    parse_version(Rest, Method, <<"*">>);
parse_uri(Buffer, Method) ->
    parse_uri_path(Buffer, Method, <<>>).

parse_uri_path(<< C, Rest/bits >>, Method, Acc) ->
    case C of
        $\r -> {error, invalid_http_message};
        $\s -> parse_version(Rest,  Method, Acc);
        _ -> parse_uri_path(Rest, Method, << Acc/binary, C >>)
    end.

parse_version(<< "HTTP/", High, ".", Low, $\r, $\n, Rest/binary >>, Method,
              URI) when High >= $0, High =< $9, Low >= $0, Low =< $9 ->
    Version = { High -$0, Low - $0},
    {ok, {http_request, Method, URI, Version}, Rest};
parse_version(_, _, _) ->
     {error, invalid_http_message}.

parse_header(Buf) ->
    case binary:split(Buf, <<"\r\n">>) of
        [<<>>, Rest] ->
            {ok, http_eoh, Rest};
        [<< " ", Line/binary >>, Rest] ->
            NewBuf = iolist_to_binary([Line, Rest]),
            parse_header1(NewBuf, <<>>);
        [<< "\t", Line/binary >>, Rest] ->
            NewBuf = iolist_to_binary([Line, Rest]),
            parse_header1(NewBuf, <<>>);
        [Line, Rest]->
            parse_header1(Line, Rest);
        [Buf] ->
            {more, undefined}
    end.


parse_header1(Line, Rest) ->
    [Key, Value] = case binary:split(Line, <<": ">>, [trim]) of
        [K] -> [K, <<>>];
        [K, V] -> [K, V]
    end,
    {ok, {http_header, Key, Value}, Rest}.


decode_chunked_body(<<>>) ->
    {ok, http_chunked_done};
decode_chunked_body(Data) ->
    case read_size(Data) of
        {ok, 0, Rest} ->
            case parse_trailers(Rest, []) of
                {ok, Trailers, Rest1} ->
                    {ok, {http_chunk_done, Trailers}, Rest1};
                _ ->
                    {ok, {http_chunk_done, []}, Rest}
            end;
        {ok, Size, Rest} ->
            case read_chunk(Rest, Size) of
                {ok, Chunk, Rest1} ->
                    {ok, {http_chunk, Chunk}, Rest1};
                eof ->
                    {more, undefined};
                Error ->
                    Error
            end;
        eof ->
            {more, undefined};
        Error ->
            Error
    end.


parse_trailers(Rest, Acc) ->
    case parse_header(Rest) of
        {ok, {http_header, K, V}, Rest2} ->
            parse_trailers(Rest2, [{K, V} | Acc]);
        {ok, http_eoh, Rest2} ->
            {ok, lists:reverse(Acc), Rest2};
        _ -> error
    end.




read_size(Data) ->
    case read_size(Data, [], true) of
        {ok, Line, Rest} ->
            case io_lib:fread("~16u", Line) of
                {ok, [Size], []} ->
                    {ok, Size, Rest};
                _ ->
                    {error, {poorly_formatted_size, Line}}
            end;
        Err ->
            Err
    end.

read_size(<<>>, _, _) ->
    eof;

read_size(<<"\r\n", Rest/binary>>, Acc, _) ->
    {ok, lists:reverse(Acc), Rest};

read_size(<<$;, Rest/binary>>, Acc, _) ->
    read_size(Rest, Acc, false);

read_size(<<C, Rest/binary>>, Acc, AddToAcc) ->
    case AddToAcc of
        true ->
            read_size(Rest, [C|Acc], AddToAcc);
        false ->
            read_size(Rest, Acc, AddToAcc)
    end.

read_chunk(Data, Size) ->
    case Data of
        <<Chunk:Size/binary, "\r\n", Rest/binary>> ->
            {ok, Chunk, Rest};
        <<_Chunk:Size/binary, _Rest/binary>> when size(_Rest) >= 2 ->
            {error, poorly_formatted_chunked_size};
        _ ->
            eof
    end.


parse_native_first_line(_Bin, _St, 0) ->
    {error, invalid_http_message};
parse_native_first_line(Bin, #hstate{max_line_length=Sz}=St, Empty) ->
    case erlang:decode_packet(http_bin, Bin, [{packet_size, Sz}]) of
        {ok, {http_response, _, _, _}, _} = Resp -> Resp;
        {ok, {http_request, _, _, _}, _} = Req -> Req;
        {ok, {http_error, <<"\r\n">>}, Rest} ->
            parse_native_first_line(Rest, St, Empty - 1);
        {ok, {http_error, <<"\n">>}, Rest} ->
            parse_native_first_line(Rest, St, Empty - 1);
        {more, _} = More -> More;
        Error -> Error
    end.


parse_native_header(Bin) ->
    case erlang:decode_packet(httph_bin, Bin, []) of
        {ok, http_eoh, Rest} ->
            {ok, http_eoh, Rest};
        {ok, {http_header, _, K, _, V}, Rest} ->
            {ok, {http_header, nplib_bstr:to_binary(K), V}, Rest};
        {ok, {http_error, <<"\r\n">>}, Rest} ->
            parse_native_header(Rest);
        {ok, {http_error, <<"\n">>}, Rest} ->
            parse_native_header(Rest);
        {more, _} = More -> More;
        Error -> Error
    end.

%% @private

parse_options([], St) ->
    St;
parse_options([auto | Rest], St) ->
    parse_options(Rest, St#hstate{type=auto});
parse_options([request | Rest], St) ->
    parse_options(Rest, St#hstate{type=request});
parse_options([response | Rest], St) ->
    parse_options(Rest, St#hstate{type=response});
parse_options([{max_line_length, MaxLength} | Rest], St) ->
    parse_options(Rest, St#hstate{max_line_length=MaxLength});
parse_options([{max_empty_lines, MaxEmptyLines} | Rest], St) ->
    parse_options(Rest, St#hstate{max_empty_lines=MaxEmptyLines});
parse_options([_ | Rest], St) ->
    parse_options(Rest, St).
