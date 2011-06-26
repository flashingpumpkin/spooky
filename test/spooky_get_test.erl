%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: Tests for GET methods
-module(spooky_get_test).

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
-import(spooky_test, [request/0, request/1, request/2, status/1]).


%%
%% API Functions
%%

spooky_test_()->
    spooky_test:setup(spooky_get_hello_world,
                      [
                       {"Server root", fun root/0},
                       {"Server path", fun path/0},
                       {"Invalid route", fun invalid_route/0},
                       {"Invalid method", fun invalid_method/0}
                      ]).

%%
%% Local Functions
%%

root()->
    ?assertEqual(200, status(request())).

path()->
    ?assertEqual(200, status(request('GET',"/androids"))).

invalid_route()->
    ?assertEqual(404, status(request('GET',"/androids/sheep"))).

invalid_method()->
    ?assertEqual(404, status(request('POST', "/androids"))).
