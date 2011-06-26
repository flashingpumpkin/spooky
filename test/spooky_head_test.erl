%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: Tests for HEAD method
-module(spooky_head_test).

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
    spooky_test:setup(spooky_hello_world,
                      [
                       {"Server root", fun root/0},
                       {"Server path", fun path/0},
                       {"Invalid route", fun invalid_route/0}
                      ]).

%%
%% Local Functions
%%

root()->
    ?assertEqual(200, status(request('HEAD'))).

path()->
    ?assertEqual(200, status(request('HEAD',"/androids"))).

invalid_route()->
    ?assertEqual(404, status(request('HEAD',"/androids/sheep"))).

