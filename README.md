# ![Spooky](http://flashingpumpkin.github.com/spooky/spooky.svg) Spooky 

## Synopsis

Spooky is a lightweight and dead easy to use RESTful request handler for 
Erlang.

It's using the [Misultin](https://github.com/ostinelli/misultin) http 
library and provides request handling similar to the 
[Sinatra](http://www.sinatrarb.com/) web framework.

*hello_world.erl*

        -module(hello_world).
        -behaviour(spooky).
        -export([init/1, get/2]).

        init([])->
            [{port, 8000}].

        get(Req, [])->
            Req:ok("Hello world.");
        get(_, ["smashingpumpkins"])->
            throw({418, "I'm a teapot."});
        get(Req, [Name])->
            Req:ok("Hello world, " ++ Name ++ ".").

*shell*

        $ make && erlc -pa ebin/ hello_world.erl && erl -pa ebin/ -pa deps/*/ebin
        [...]
        1> spooky:start_link(hello_world)
        {ok, <0.40.0>}
        2> spooky:stop()
        true

## Features

* Modular  
  Split your web application up into different, reusable modules. Say,
  for user management, mnesia administration or login logic.

* RESTful request handling/routing
    - Use get/post/put/delete/head methods for a clean seperation of 
      your HTTP API
    - Use pattern matching to handle requests to different URLs

* Middlewares  
  Use chained middlewares to preprocess the request before it arrives 
  at your handlers and modify or abort it on the fly.

* Erlang  
  Dead simple interfacing to your `gen_servers`, `gen_fsms` and 
  `gen_events`

## Todo

* Write documentation

## Why?

Scratching an itch. Spooky is as simple as it gets - you're left to deal
with everything else than request handling. If this is not what you are
looking for and need things like ORM support, templating, etc, you should
look at other frameworks:

* [Nitrogen](http://nitrogenproject.com/)
* [Erlang Web](http://www.erlang-web.org/)
* [Chicago Boss](http://www.chicagoboss.org/)
* [Zotonic](http://zotonic.com/)

## Thanks

Thanks to [Learn you some Erlang](http://www.learnyousomeerlang), 
[Mochiweb](https://github.com/mochi/mochiweb), 
[Misultin](https://github.com/ostinelli/misultin) and
[CouchDB](http://couchdb.org/) for being excellent repositories to learn
about Erlang.
