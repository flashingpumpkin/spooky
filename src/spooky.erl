%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: TODO: Add description to spooky
-module(spooky).
-vsn("0.1").
-behaviour(supervisor).

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
    exit(Pid, normal).

%% 
%% Setup functions
%% 
start(Module) when is_atom(Module)->
    start([Module], []);
start(Modules) when is_list(Modules)->
    start(Modules, []).

start([Module|T], Handlers)->
    % The first module's options are the ones for misultin
    Opts = apply(Module, init, [[]]),
    
    Opts0 = proplists:delete(handlers, Opts),
    start([Module|T], Handlers, Opts0).

start([Module|T], Handlers, Opts)->
    % Accumulate all the handlers
    ModuleOpts = apply(Module, init, [[]]),
    
    case proplists:lookup(handlers, ModuleOpts)of
        none ->
            ModuleHandlers = [Module];
        {handlers, ModuleHandlers}->
            ModuleHandlers = ModuleHandlers
    end,
    start(T, Handlers ++ ModuleHandlers, Opts);
start([], Handlers, Opts)->
    % Start the supervisor
    Loop = fun(Req)-> spooky_server:handle(Req, Handlers) end,
    Opts0 = Opts ++ [{loop, Loop}],
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Handlers, Opts0]).

%%
%% Supervisor callback
%% 
init([Handlers, Opts])->
    % misultin specs
    MisultinSpecs = {misultin,
                     {misultin, start_link, [Opts]},
                     permanent, infinity, supervisor, [misultin]
                    },  
    % spooky specs
    ServerSpecs = {spooky_server,
                   {spooky_server, start_link, [Handlers]},
                   permanent, 5000, worker, [spooky_server] 
                  },    
    
    {ok, { {one_for_all, 5, 10}, [MisultinSpecs, ServerSpecs]} }.

%%
%% Behaviour information
%%
behaviour_info(callbacks)->
    [{init, 1}];
behaviour_info(_Other)->
    undefined.

