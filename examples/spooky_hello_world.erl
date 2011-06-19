%% Author: Alen Mujezinovic
%% Created: 18 Jun 2011
%% Description: TODO: Add description to simple_hello_world
-module(spooky_hello_world).
-behaviour(spooky).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/1, get/2,post/2,put/2,delete/2,head/2]).

%%
%% API Functions
%%

init([])->
    [{port, 8000}].

get(Req,[])->
    Req:ok("Hello world");
get(_Req,["teapot"])->
    {redirect, "/smashingpumpkins"};
get(_Req, ["smashingpumpkins"])->
    throw(418);
get(Req,[Name])->
    Req:ok("Hello world, " ++ Name ++ ".").

post(Req,[])->
    Req:ok("Hello world");
post(_Req, ["smashingpumpkins"])->
    throw({418, "I'm a teapot."});
post(Req,[Name])->
    Req:ok("Hello world, " ++ Name ++ ".").

put(Req,[])->
    Req:ok("Hello world");
put(_Req, ["smashingpumpkins"])->
    throw({418, [{"X-Server", "Teapot"}], "I'm a teapot."});
put(Req,[Name])->
    Req:ok("Hello world, " ++ Name ++ ".").

delete(Req,[])->
    Req:ok("Hello world");
delete(Req,[Name])->
    Req:ok("Hello world, " ++ Name ++ ".").

head(Req,[])->
    Req:ok("Hello world");
head(Req,[Name])->
    Req:ok("Hello world, " ++ Name ++ ".").
