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


response(StatusCode)->
    receive 
        {response, Status, _, _} ->
            ?assert(Status =:= StatusCode)
    after 
        200 ->
            ?assert(timeout)
    end.

response(StatusCode, Template)->
    receive 
        {response, Status, _, Response} when is_list(Response)->
            ?assert(Status =:= StatusCode),
            ?assert(string:equal(Template, Response))            
    after
        200 ->
            ?assert(timeout)
    end.
            
response(StatusCode, Headers, Template)->
    receive 
        {response, Status, ResponseHeaders, Response} when is_list(Response)->
            ?assert(Status =:= StatusCode),
            ?assert(string:equal(Template, Response)),
            ?assert(Headers =:= ResponseHeaders)
    after
        200 ->
            ?assert(timeout)
    end.
                

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
    spooky_server:handle(request()),
    response(200),
    spooky_server:handle(request('GET', "/androids")),
    response(200),
    spooky_server:handle(request('GET', "/androids/sheep")),
    response(404),
    spooky_server:handle(request('POST', "/androids")),
    response(404),
    stop().

redirect_test()->
    spooky:start_link(spooky_hello_world),
    spooky_server:handle(request('GET', "/teapot")),
    response(301),
    stop().

post_test()->
    spooky:start_link(spooky_post_hello_world),
    spooky_server:handle(request('POST')),
    response(200),
    spooky_server:handle(request('POST', "/androids")),
    response(200),
    spooky_server:handle(request('POST', "/androids/sheep")),
    response(404),
    spooky_server:handle(request('GET', "/androids")),
    response(404),
    stop().

put_test()->
    spooky:start_link(spooky_hello_world),
    spooky_server:handle(request('PUT')),
    response(200),
    spooky_server:handle(request('PUT', "/androids")),
    response(200),
    spooky_server:handle(request('PUT', "/androids/sheep")),
    response(404),
    stop().

delete_test()->
    spooky:start_link(spooky_hello_world),
    spooky_server:handle(request('DELETE')),
    response(200),
    spooky_server:handle(request('DELETE', "/androids")),
    response(200),
    spooky_server:handle(request('DELETE', "/androids/sheep")),
    response(404),
    stop().

head_test()->
    spooky:start_link(spooky_hello_world),
    spooky_server:handle(request('HEAD')),
    response(200),
    spooky_server:handle(request('HEAD', "/androids")),
    response(200),
    spooky_server:handle(request('HEAD', "/androids/sheep")),
    response(404),
    stop().

multi_test()->
    spooky:start_link(spooky_multi_hello_world),
    spooky_server:handle(request()),
    response(200),
    spooky_server:handle(request('POST')),
    response(200),
    spooky_server:handle(request('PUT')),
    response(404),
    stop().

errors_test()->
    spooky:start_link(spooky_hello_world),
    spooky_server:handle(request('GET', "/smashingpumpkins")),
    response(418),
    spooky_server:handle(request('POST', "/smashingpumpkins")),
    response(418, "I'm a teapot."),
    spooky_server:handle(request('PUT', "/smashingpumpkins")),
    response(418, [{"X-Server", "Teapot"}], "I'm a teapot."),
    stop().