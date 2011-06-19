%%% -------------------------------------------------------------------
%%% Author  : Alen Mujezinovic
%%% Description :
%%%
%%% Created : 18 Jun 2011
%%% -------------------------------------------------------------------
-module(spooky_server).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, handle/1, handlers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {handlers}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Handlers)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Handlers], []).

handle(Req)->
    io:format("Handling request~n"),
    Handlers = handlers(),
    Method = Req:get(method),
    handle(Req, Method, Handlers).

handlers()->
    gen_server:call(?MODULE, handlers).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Handlers]) ->
    io:format("Started process with handlers ~p ~n", [Handlers]),
    {ok, #state{handlers=Handlers}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(handlers, _From, State=#state{handlers=Handlers})->
    {reply, Handlers, State};
handle_call(_Request, _From, _State) ->
    _Reply = ok,
    {reply, _Reply, _State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, _State) ->
    {noreply, _State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, _State) ->
    {noreply, _State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


handle(Req, _Method, [], _Path)->
    Req:respond(404);
handle(Req, Method, [Handler|T], Path)->
    io:format("Trying handler ~p ~n", [Handler]),
    try apply(Handler, Method, [Req, Path]) of
         _ -> 
             ok
    catch 
        error:undef ->
            io:format("Undef error ~n"),
            handle(Req, Method, T, Path);
        error:function_clause ->
            io:format("Function clause error ~n"),
            handle(Req, Method, T, Path);
        Status when is_number(Status)->
            Req:respond(Status);
        {Status, Template} when is_number(Status) and is_list(Template)->
            Req:respond(Status, Template);
        {Status, Headers, Template} when is_number(Status) and is_list(Headers) and is_list(Template)->
            Req:respond(Status, Headers, Template)
    end.

handle(Req, 'GET', Path)->
    handle(Req, get, Path);
handle(Req, 'POST', Path)->
    handle(Req, post, Path);
handle(Req, 'PUT', Path)->
    handle(Req, put, Path);
handle(Req, 'DELETE', Path)->
    handle(Req, delete, Path);
handle(Req, 'HEAD', Path)->
    handle(Req, head, Path);
handle(Req, Method, Handlers)->
    io:format("Handling method ~p with handlers ~p ~n", [Method, Handlers]),
    Path = Req:resource([lowercase, urldecode]),
    handle(Req, Method, Handlers, Path).
