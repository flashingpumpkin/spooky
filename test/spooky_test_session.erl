-module(spooky_test_session).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/misultin/include/misultin.hrl").



request(Method, Uri)->
    {misultin_req, #req{uri={type, Uri}, headers=[], method=Method}, self()}.

request(Method, Uri, Cookie)->
    {misultin_req, 
     #req{uri={type, Uri}, method=Method, headers=[{'Cookie', Cookie}]}, 
     self()}.



start()->
    spooky:start_link(spooky_session_example).

stop()->
    true = spooky:stop().

session_test()->
    start(),
    
    {response, Status, Headers, Body} = spooky_server:handle(request(get, "/")),
    io:format("~p, ~p, ~p", [Status, Headers, Body]),
    ?assertEqual(Status, 200),
    ?assertEqual(Body, ["1"]),

    Cookie = proplists:get_value("Set-Cookie", Headers, none),
    
    {_, Status0, _, Body0} = spooky_server:handle(request(get,"/increment", Cookie)),
    ?assertEqual(Status0, 200),
    ?assertEqual(Body0, "Incremented"),
    
    {_, _, _, Body1} = spooky_server:handle(request(get, "/", Cookie)),
    ?assertEqual(Body1, ["2"]),
    
    {_, Status2, _, Body2} = spooky_server:handle(request(get,"/decrement", Cookie)),
    ?assertEqual(Status2, 200),
    ?assertEqual(Body2, "Decremented"),

    {_, _, _, Body3 } = spooky_server:handle(request(get,"/", Cookie)),
    ?assertEqual(Body3, ["1"]),
    
    stop().