%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: TODO: Add description to spooky_multi_test
-module(spooky_multi_test).
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
    spooky_test:setup(spooky_multi_hello_world,
                      [{"GET request", fun get/0},
                       {"POST request", fun post/0},
                       {"HEAD request", fun head/0}]).

%%
%% Local Functions
%%

get()->
    ?assertEqual(200, status(request())).

post()->
    ?assertEqual(200, status(request('POST'))).

head()->
    ?assertEqual(404, status(request('HEAD'))).