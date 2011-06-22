%% Author: Alen Mujezinovic
%% Created: 22 Jun 2011
%% Description: TODO: Add description to spooky_state
-module(spooky_state).

%%
%% Include files
%%

-include("../include/spooky.hrl").

%%
%% Exported Functions
%%
-export([create/0]).

%%
%% API Functions
%%

create()->
    ?LOG_INFO("Creating new response state from request.", []),
    Dict = dict:new(),
    Dict0 = Dict:store(cookies, []),
    Dict1 = Dict0:store(headers, []),
    Dict1.



%%
%% Local Functions
%%

