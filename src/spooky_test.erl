%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: TODO: Add description to spooky_test
-module(spooky_test).

%%
%% Include files
%%

-include_lib("misultin/include/misultin.hrl").

%%
%% Exported Functions
%%
-export([setup/2]).
-export([request/0, request/1, request/2, request/3]).
-export([status/1, headers/1, body/1]).

%%
%% API Functions
%%

request()->
    request('GET').

request(Method)->
    request(Method, "/").

request(Method, Uri)->
    request(Method, Uri, []).

request(Method, Uri, Headers)->
    Req = {misultin_req,
           #req{uri={type, Uri}, method=Method, headers=Headers},
           self()},
    spooky_server:handle(Req).


status({response, Status, _, _})->
    Status.

body({response, _, _, Body})->
    Body.

headers({response, _, Headers, _})->
    Headers.

setup(Handler, Tests)->
    {setup,
     fun()->
             spooky:start_link(Handler)
     end,
     fun(_)-> 
             spooky:stop()
     end,
     Tests}.
%%
%% Local Functions
%%

