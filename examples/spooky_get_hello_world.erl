%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: This file shows how to split an application up into multiple
%%              files. See `example/multi_hello_world.erl`
-module(spooky_get_hello_world).
-behaviour(spooky).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/1, get/3]).

%%
%% API Functions
%%

init([])->
    [{port, 8008}, {handlers, [?MODULE]}].

get(_Req, [], _State)->
    {200, "Hello world"};
get(_Req, [Name], _State)->
    {200, "Hello world, " ++ Name}.

%%
%% Local Functions
%%

