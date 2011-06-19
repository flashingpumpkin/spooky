%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: This file shows how to split an application up into multiple
%%              files.
-module(spooky_multi_hello_world).
-behaviour(spooky).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/1]).

%%
%% API Functions
%%

init([])->
    [{port, 8000}, {handlers, [spooky_get_hello_world, spooky_post_hello_world]}].
