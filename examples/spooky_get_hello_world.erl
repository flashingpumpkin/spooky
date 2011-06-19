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
-export([init/1, get/2]).

%%
%% API Functions
%%

init([])->
    [{port, 8000}, {handlers, [?MODULE]}].

get(Req, [])->
    Req:ok("Hello world.");
get(Req, [Name])->
    Req:ok("Hello world, " ++ Name ++ ".").

%%
%% Local Functions
%%

