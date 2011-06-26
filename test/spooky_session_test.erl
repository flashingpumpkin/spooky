%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: Tests for session support
-module(spooky_session_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").


%%
%% Exported Functions
%%
-export([spooky_test_/0]).

%% 
%% Imported Functions
%% 
-import(spooky_test, [request/2, request/3, 
                      status/1, 
                      body/1]).


%%
%% API Functions
%%

spooky_test_()->
    spooky_test:setup(spooky_session_example,
                      [
                       {"Session test", fun session/0}
                      ]).

%%
%% Local Functions
%%

session()->
    {response, Status, Headers, _} = request('GET', "/"),
    ?assertEqual(200, Status),
    
    Cookie = proplists:get_value("Set-Cookie", Headers, []),
    SH = [{'Cookie', Cookie}],
    ?assertEqual("Incremented", body(request('GET', "/increment", SH))),
    ?assertEqual("Decremented", body(request('GET', "/decrement", SH))),
    ?assertEqual(["1"], body(request('GET', "/", SH))).
