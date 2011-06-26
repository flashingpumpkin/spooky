%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: Testing the redirect
-module(redirect_test).


%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([spooky_test_/0]).

%% 
%% Imported Functions
%% 
-import(spooky_test, [request/0, request/1, request/2, status/1]).


%%
%% API Functions
%%

spooky_test_()->
    spooky_test:setup(spooky_hello_world,
                      [{"Redirect test", fun redirect/0}]).

%%
%% Local Functions
%%

redirect()->
    ?assertEqual(301, status(request('GET',"/teapot"))).
