-module(spooky_test_template).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/misultin/include/misultin.hrl").

request(Method, Uri)->
    {misultin_req, #req{uri={type, Uri}, headers=[], method=Method}, self()}.

template_test()->
    spooky:start_link(spooky_template_example),
    
    {response, Status, Headers, Body} = spooky_server:handle(request(get,"/")),
    
    ?assertEqual(200, Status),
    ?assertEqual(["<html>
        <head><title>Spooky template example</title></head>
        <body>", 
        "Hello world", 
        "</body>
        </html>"], Body),
    
    spooky:stop().
