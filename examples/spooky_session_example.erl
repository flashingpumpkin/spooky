%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: This file shows how to split an application up into multiple
%%              files. See `example/multi_hello_world.erl`
-module(spooky_session_example).
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
    SessionInit = spooky_sessions:init([]),
    
    [{port, 8000}, {handlers, [?MODULE]}] ++ SessionInit.

get(_Req, [], State)->
    Session = State:fetch(session),

    case Session:find(counter) of 
        error ->
            Counter = 1;
        _ ->
            Counter = Session:fetch(counter)
    end,
    
    _Session0 = Session:store(counter, Counter),
    
    {200, io_lib:format("~p", [Counter])};

get(Req, [Operation], State)->
    Session = State:fetch(session),
    Counter = Session:fetch(counter),
    
    Store = fun(Val)->
                    Session:store(counter, Counter + Val)
            end,
    get(Req, Operation, State, Store).

get(_, "increment", _, Store)->
    Store(+1),
    {200, "Incremented"};
get(_, "decrement", _, Store)->
    Store(-1),
    {200, "Decremented"}.

%%
%% Local Functions
%%

