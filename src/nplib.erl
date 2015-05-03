%%% -*- erlang -*-
%%%
%%% This file is part of nplib released under the MIT license
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2015 Benoit Chesneau
%%%
-module(nplib).

-export([decode_packet/3]).

%% @doc Decodes the binary Bin according to the packet protocol specified by
%% Type. It also wraps `erlang:decode_packet/3' for missing protocols.
%%
%% Packet types are:
%% <ul>
%% <li>`asn1' - ASN.1 BER</li>
%% <li>`sunrm' - Sun's RPC encoding</li>
%% <li>`cdr' - CORBA (GIOP 1.1)</li>
%% <li>`fcgi' - Fast CGI</li>
%% <li>`tpkt' - TPKT format [RFC1006]</li>
%% <li>`http | httph | http_chunked' : HTTP 1.1 parsing. It uses a full erlang
%% parser or the parser from the standard library if the native option is
%% given</li>
%% </ul>
decode_packet(http, Bin, Opts) ->
    nplib_http:decode(http, Bin, Opts);
decode_packet(httph, Bin, Opts) ->
    nplib_http:decode(httph, Bin, Opts);
decode_packet(http_bin, Bin, Opts) ->
    nplib_http:decode(http, Bin, Opts);
decode_packet(httph_bin, Bin, Opts) ->
    nplib_http:decode(httph, Bin, Opts);
decode_packet(http_chunked, Bin, Opts) ->
    nplib_http:decode(http_chunked, Bin, Opts);
decode_packet(Type, Bin, Opts) ->
    erlang:decode_packet(Type, Bin, Opts).

