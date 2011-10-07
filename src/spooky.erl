%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: TODO: Add description to spooky
-module(spooky).
-vsn("0.1").
-behaviour(supervisor).

-include("../include/spooky.hrl").

%%
%% Exported Functions
%%
-export([start_link/1, init/1, stop/0, behaviour_info/1]).

%%
%% API Functions
%%
start_link(Module) when is_atom(Module)->
    start(Module);
start_link(Modules) when is_list(Modules)->
    start(Modules).

stop()->
    stop(?MODULE).
stop(Name) when is_atom(Name) ->
    stop(whereis(Name));
stop(Pid) when is_pid(Pid)->
    ?LOG_INFO("Stopping spooky with pid ~p", [Pid]),
    exit(Pid, normal).

%% 
%% Setup functions
%% 
start(Module) when is_atom(Module)->
    start([Module], [], []);
start(Modules) when is_list(Modules)->
    start(Modules, [], []).

start([Module|T], Handlers, Middlewares)->
    ?LOG_INFO("Starting spooky with ~p", [Module]),
    % The first module's options are the ones for misultin
    {ModuleOpts, ModuleHandlers, ModuleMiddlewares} = init_module(Module),
    Opts = proplists:delete(handlers, ModuleOpts),
    ?LOG_INFO("Options ~p", [ModuleOpts]),
    start(T, Handlers ++ ModuleHandlers, Middlewares ++ ModuleMiddlewares, Opts).

start([Module|T], Handlers, Middlewares, Opts)->
    % Accumulate all the handlers and middlewares
    {_, ModuleHandlers, ModuleMiddlewares} = init_module(Module),
    start(T, Handlers ++ ModuleHandlers, Middlewares ++ ModuleMiddlewares, Opts);

start([], Handlers, Middlewares, Opts)->
    % Start the supervisor
    ?LOG_INFO("Handlers ~p", [Handlers]),
    ?LOG_INFO("Middlewares ~p", [Middlewares]),
    Loop = fun(Req)-> 
                   spooky_server:handle(Req, Handlers, Middlewares) 
           end,
    Opts0 = Opts ++ [{loop, Loop}],
    ?LOG_INFO("Starting supervisor", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Handlers, Middlewares, Opts0]).

%%
%% Supervisor callback
%% 
init([Handlers, Middlewares, Opts])->
    % misultin specs
    MisultinSpecs = {misultin,
                     {misultin, start_link, [Opts]},
                     permanent, infinity, supervisor, [misultin]
                    },  
    % spooky specs
    ServerSpecs = {spooky_server,
                   {spooky_server, start_link, [Handlers, Middlewares]},
                   permanent, 5000, worker, [spooky_server] 
                  },    
    ?LOG_DEBUG("Starting servers",[]),
    {ok, { {one_for_all, 5, 10}, [MisultinSpecs, ServerSpecs]} }.

%%
%% Behaviour information
%%
behaviour_info(callbacks)->
    [{init, 1}];
behaviour_info(_Other)->
    undefined.


%%
%% Utility methods
%%
init_module(Module) ->
    Opts = apply(Module, init, [[]]),
    Handlers = lookup(handlers, Opts, [Module]),
    Middlewares = lookup(middlewares, Opts, []),
    {Opts, Handlers, Middlewares}.

lookup(Key, Opts, Default)->
    case proplists:lookup(Key, Opts) of
        none -> 
            Default;
        {Key, Value} ->
            Value
    end.
