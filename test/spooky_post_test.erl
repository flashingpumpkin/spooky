%% Author: Alen Mujezinovic 
%% Created: 26 Jun 2011
%% Description: Testing POST methods
-module(spooky_post_test).
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
    spooky_test:setup(spooky_post_hello_world,
                      [{"Server root", fun root/0},
                       {"Server path", fun path/0},
                       {"Invalid route", fun invalid_route/0},
                       {"Invalid method", fun invalid_method/0}]).

%%
%% Local Functions
%%

root()->
    ?assertEqual(200, status(request('POST'))).

path()->
    ?assertEqual(200, status(request('POST', "/androids"))).

invalid_route()->
    ?assertEqual(404, status(request('POST', "androids/sheep"))).

invalid_method()->
    ?assertEqual(404, status(request('GET'))).