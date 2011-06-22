-module(spooky_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/misultin/include/misultin.hrl").


request()->
    {misultin_req, #req{uri={type, "/"}, method='GET'}, self()}.

request(Method)->
    {misultin_req, #req{uri={type, "/"}, method=Method}, self()}.

request(Method, Uri)->
    {misultin_req, #req{uri={type, Uri}, method=Method}, self()}.


response(ResponseStatus, {response, Status, _, _})->
    ?assertEqual(ResponseStatus, Status).

status({response, Status, _, _})->Status.

stop()->
    true = spooky:stop().

start_atom_test()->
    {ok, _Pid} = spooky:start_link(spooky_hello_world),
    stop().

start_list_test()->
    {ok, _Pid} = spooky:start_link([spooky_get_hello_world, spooky_post_hello_world]),
    stop().

stop_test()->
    {ok, _Pid} = spooky:start_link([spooky_get_hello_world]),
    stop(),
    case catch spooky_server:handle(request()) of
        {'EXIT', {shutdown, _}} ->
            ok        
    end.

get_test()->
    spooky:start_link(spooky_get_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request()))),
    ?assertEqual(200, status(spooky_server:handle(request('GET', "/androids")))),
    ?assertEqual(404, status(spooky_server:handle(request('GET', "/androids/sheep")))),
    ?assertEqual(404, status(spooky_server:handle(request('POST', "/androids")))),
    stop().

redirect_test()->
    spooky:start_link(spooky_hello_world),
    ?assertEqual(301, status(spooky_server:handle(request('GET', "/teapot")))),
    stop().

post_test()->
    spooky:start_link(spooky_post_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request('POST')))),
    ?assertEqual(200, status(spooky_server:handle(request('POST', "/androids")))),
    ?assertEqual(404, status(spooky_server:handle(request('POST', "/androids/sheep")))),
    ?assertEqual(404, status(spooky_server:handle(request('GET', "/androids")))),
    stop().
 
put_test()->
    spooky:start_link(spooky_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request('PUT')))),
    ?assertEqual(200, status(spooky_server:handle(request('PUT', "/androids")))),
    ?assertEqual(404, status(spooky_server:handle(request('PUT', "/androids/sheep")))),
    stop().


delete_test()->
    spooky:start_link(spooky_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request('DELETE')))),
    ?assertEqual(200, status(spooky_server:handle(request('DELETE', "/androids")))),
    ?assertEqual(404, status(spooky_server:handle(request('DELETE', "/androids/sheep")))),
    stop().

head_test()->
    spooky:start_link(spooky_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request('HEAD')))),
    ?assertEqual(200, status(spooky_server:handle(request('HEAD', "/androids")))),
    ?assertEqual(404, status(spooky_server:handle(request('HEAD', "/androids/sheep")))),
    stop().

multi_test()->
    spooky:start_link(spooky_multi_hello_world),
    ?assertEqual(200, status(spooky_server:handle(request()))),
    ?assertEqual(200, status(spooky_server:handle(request('POST')))),
    ?assertEqual(404, status(spooky_server:handle(request('PUT')))),
    stop().

errors_test()->
    spooky:start_link(spooky_hello_world),
    ?assertEqual(418, status(spooky_server:handle(request('GET', "/smashingpumpkins")))),
    ?assertEqual(418, status(spooky_server:handle(request('POST', "/smashingpumpkins")))),
    ?assertEqual(418, status(spooky_server:handle(request('PUT', "/smashingpumpkins")))),
    stop().