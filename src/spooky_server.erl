%%% -------------------------------------------------------------------
%%% Author  : Alen Mujezinovic
%%% Description :
%%%
%%% Created : 18 Jun 2011
%%% -------------------------------------------------------------------
-module(spooky_server).
-vsn("0.1").
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("../include/spooky.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, handle/1, handle/3, handlers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Handlers, Middlewares)->
    ?LOG_INFO("Starting server", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Handlers, Middlewares], []).

handle(Req)->
    Handlers = handlers(),
    Middlewares = middlewares(),
    handle(Req, Handlers, Middlewares).

handle(Req, Handlers, Middlewares)->
    Method = Req:get(method),
    handle(Req, Method, Handlers, Middlewares).

handlers()->
    gen_server:call(?MODULE, handlers).
middlewares()->
    gen_server:call(?MODULE, middlewares).

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
init([Handlers, Middlewares]) ->
    {ok, [Handlers, Middlewares]}.

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
handle_call(handlers, _From, [Handlers, Middlewares])->
    {reply, Handlers, [Handlers, Middlewares]};
handle_call(middlewares, _From, [Handlers, Middlewares])->
    {reply, Middlewares, [Handlers, Middlewares]};
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


handle(Req, 'GET', Handlers, Middlewares)->
    handle(Req, get, Handlers, Middlewares);
handle(Req, 'POST', Handlers, Middlewares)->
    handle(Req, post, Handlers, Middlewares);
handle(Req, 'PUT', Handlers, Middlewares)->
    handle(Req, put, Handlers, Middlewares);
handle(Req, 'DELETE', Handlers, Middlewares)->
    handle(Req, delete, Handlers, Middlewares);
handle(Req, 'HEAD', Handlers, Middlewares)->
    handle(Req, head, Handlers, Middlewares);
handle(Req, Method, Handlers, Middlewares)->
    Path = Req:resource([lowercase, urldecode]),
    handle(Req, Method, Path, Handlers, Middlewares).

respond(Req, Status, State) when is_number(Status)->
    respond(Req, Status, [], [], State).
respond(Req, Status, Headers, Body)->
    respond(Req, Status, Headers, Body, []).
respond(Req, Status, Headers, Body, State)->
    CookieMapper = fun(Cookie)->
                           case Cookie of
                               {Key, Value} ->
                                   Req:set_cookie(Key, Value);
                               {Key, Value, Options} ->
                                   Req:set_cookie(Key, Value, Options)
                           end
                   end,
    
    Cookies = lists:map(CookieMapper, State:fetch(cookies)),
    
    Headers0 = State:fetch(headers),
    Headers1 = Headers ++ Headers0 ++ Cookies,   
   
    Req:respond(Status, Headers1, Body).

render(Req, Template, Data, State)->
    {ok, Result} = Template:render(Data),
    respond(Req, 200, [], Result, State).

% Iterate through a list of funcs until one returns a result or throws 
% an HTTP error. Note that the list of funcs are closures holding the HTTP
% method and path.
continue(Req, Funcs)->
    State = spooky_state:create(),
    continue(Req, Funcs, State).

continue(Req, [], State)->
    respond(Req, 404, State);
continue(Req, [Func|Funcs], State)->
    try Func(State) of 
        {response, Status, Headers, Body} ->
            {response, Status, Headers, Body};
        {respond, Status, Headers, Body} ->
            respond(Req, Status, Headers, Body, State);
        {respond, Status, Headers, Body, State} ->
            respond(Req, Status, Headers, Body, State);
        {redirect, Url }->
            respond(Req, 301, [{"Location", Url}], [], State);
        {render, Template, Data} ->
            render(Req, Template, Data, State);
        {Status, Body} when is_number(Status)->
            respond(Req, Status, [], Body, State);
        State0 ->
            continue(Req, Funcs, State0)
    catch 
        error:undef ->
            continue(Req, Funcs, State);
        error:function_clause ->
            continue(Req, Funcs, State);
        Status when is_number(Status)->
            respond(Req, Status, [], [], State);
        {Status, Body} when is_number(Status) and is_list(Body)->
            respond(Req, Status, [], Body, State);
        {Status, Headers, Body} when is_number(Status) and is_list(Headers) and is_list(Body)->
            respond(Req, Status, Headers, Body, State)
    end.
        

handle(Req, Method, Path, Handlers, Middlewares)->
    ?LOG_INFO("Handle: ~p ~p", [Method, Path]),
    StatelessHandler = fun(Func)->
                               fun(Handler)->
                                       fun(_State)->
                                               apply(Handler, Func, [Req, Path])
                                       end
                               end
                       end,
    
    StatefulHandler = fun(Func)->
                              fun(Handler)->
                                      fun(State)->
                                              apply(Handler, Func, [Req, Path, State])
                                      end
                              end
                      end,
    
    ?LOG_INFO("Creating middleware handlers ~p", [Middlewares]),    
    MiddlewareHandlers = lists:map(StatefulHandler(process_request), 
                                   Middlewares),
    
    ?LOG_INFO("Creating stateless handlers ~p", [Handlers]),
    StatelessRequestHandlers = lists:map(StatelessHandler(Method),
                                         Handlers),
    
    ?LOG_INFO("Creating stateful handlers ~p", [Handlers]),
    StatefulRequestHandlers = lists:map(StatefulHandler(Method),
                                        Handlers),
    
    ?LOG_INFO("Creating request handlers ~p", [Handlers]),
    RequestHandlers = lists:flatten(lists:map(fun({Less, Full})->
                                                      [Less,Full] 
                                              end,
                                              lists:zip(StatelessRequestHandlers,
                                                        StatefulRequestHandlers))),
    
    ?LOG_INFO("Starting continue loop", []),
    continue(Req, MiddlewareHandlers ++ RequestHandlers).
        
