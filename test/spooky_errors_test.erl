%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: TODO: Add description to spooky_errors_test
-module(spooky_errors_test).
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
                       {"Test error responses", fun errors/0}
                      ]).

%%
%% Local Functions
%%

errors()->
    ?assertEqual(418, status(request('GET', "/smashingpumpkins"))),
    ?assertEqual(418, status(request('POST', "/smashingpumpkins"))),
    ?assertEqual(418, status(request('PUT', "/smashingpumpkins"))).