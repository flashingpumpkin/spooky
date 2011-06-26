%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: Tests for template rendering
-module(spooky_template_test).

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
    spooky_test:setup(spooky_template_example,
                      [
                       {"Template test", fun template/0}
                      ]).

%%
%% Local Functions
%%

template()->
    {response, Status, _, Body} = request('GET', "/"),
    ?assertEqual(200, Status),
    ?assertEqual(["<html>
        <head><title>Spooky template example</title></head>
        <body>",
                  "Hello world",
        "</body>
        </html>"], Body).