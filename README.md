# Spooky

## Synposis

Spooky is a lightweight and dead easy to use RESTful request handler for 
Erlang.

It's using the [Misultin](https://github.com/ostinelli/misultin) http 
library and provides RESTful request handling similar to the 
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

        $ make && erlc -pa ebin/ hello_world.erl && erl -pa ebin/
        [...]
        1> spooky:start_link(hello_world)
        {ok, <0.40.0>}
        2> spooky:stop()
        true

## Why?

Scratching an itch. Spooky is as simple as it gets - you're left to deal
with everything else than request handling. If this is not what you are
looking for and need things like ORM support, templating, etc, you should
look at other frameworks:

* [Nitrogen](http://nitrogenproject.com/)
* [Erlang Web](http://www.erlang-web.org/)
* [Chicago Boss](http://www.chicagoboss.org/)
* [Zotonic](http://zotonic.com/)
