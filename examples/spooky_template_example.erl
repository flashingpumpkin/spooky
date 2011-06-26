%% Author: Alen Mujezinovic
%% Created: 26 Jun 2011
%% Description: TODO: Example of rendering responses with erlydtl.
-module(spooky_template_example).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get/2, init/1]).

%%
%% API Functions
%%

init([])->
    Template = 
        <<"<html>
        <head><title>Spooky template example</title></head>
        <body>{{ message }}</body>
        </html>">>,
    
    erlydtl:compile(Template, template),
    
    [{port, 8000}, {handlers, [?MODULE]}].

get(Req, Path)->
    {render, template, [{message, "Hello world"}]}.



%%
%% Local Functions
%%

