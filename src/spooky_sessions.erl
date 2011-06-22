%% Author: Alen Mujezinovic
%% Created: 22 Jun 2011
%% Description: TODO: Add description to spooky_sessions
-module(spooky_sessions).
-behaviour(spooky).

-compile(export_all).
%%
%% Include files
%%

-include("../include/spooky.hrl").

-record(spooky_sessions, {id, data}).

%%
%% Exported Functions
%%
-export([init/1, process_request/3, store/3, fetch/2, find/2]).

%%
%% API Functions
%%

init([])->
    ?LOG_INFO("Starting session", []),
    
    application:start(mnesia),

    try  mnesia:create_table(spooky_sessions, [{attributes, [id, data]}]) 
    catch {aborted, {already_exists, spooky_sessions}} -> ok end,
    
    [{handlers, [?MODULE]}, {middlewares, [?MODULE]}].

process_request(Req, _Path, State)->
    SessionId = get_id(Req),
    Session = get_session(SessionId),
    State0 = State:append(cookies, {sessionid, SessionId}),
    State0:store(session, Session).

store(Key, Value, {?MODULE, {SessionId, Data}})->
    Data0 = Data:store(Key, Value),
    
    Session = #spooky_sessions{id=SessionId, data=Data0},
    
    mnesia:transaction(fun()->
                               mnesia:write(Session)
                       end),
    
    {?MODULE, {SessionId, Data0}}.

fetch(Key, {?MODULE, {_, Data}})->
    Data:fetch(Key).

find(Key, {?MODULE, {_, Data}})->
    Data:find(Key).

get_id(Req)->
    case Req:get_cookie_value("sessionid", Req:get_cookies()) of
        undefined ->
            uuid:to_string(uuid:v4());            
        SessionId ->
            SessionId
    end.

get_session(SessionId)->
    Transaction = fun()->
                          mnesia:match_object({?MODULE, SessionId, '_'})
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, [#spooky_sessions{id=SessionId, data = Data}]} ->
            Session = {SessionId, Data};
        {atomic, []} ->
            Session = {SessionId, dict:new()}
    end,
    {?MODULE, Session}.


%%
%% Local Functions
%%

