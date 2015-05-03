-module(nplib_http_tests).
-include_lib("eunit/include/eunit.hrl").

parse_response_correct_200_test() ->
	Response = <<"HTTP/1.1 200 OK\r\n\r\n">>,

	{ok,
     {http_response, _Version, StatusInt, Reason},_Rest} = nplib_http:decode(http, Response, []),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"OK">>).
