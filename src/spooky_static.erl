-module(spooky_static).

-behaviour(spooky).

-export([init/1, get/2]).

init(_)->
    [{ handlers, [?MODULE]}].


get(_, ["static" | Path ]) ->

    Relname = filename:join("./static", filename:join(Path)),
    Absname = normpath(filename:absname(Relname)),
    io:format("Absname: ~p~n" , [Absname]),
    case string:str(Absname, filename:absname("./static")) of
        1 ->

            case file:read_file(Absname) of
                { ok, Data }->

                    { 200, binary_to_list(Data)};
		_ ->                    { 404, "No such file" }
            end;
        _Foo ->
            { 403, "Forbidden" }
    end.


normpath(Path) ->
    filename:join(normpath(filename:split(Path), [])).
normpath([], Acc) ->
    lists:reverse(Acc);
normpath([".." | Rest], [_|Acc]) ->
    normpath(Rest, Acc);
normpath([Head|Rest], Acc) ->
    normpath(Rest, [Head|Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
normpath_test()->
    ?assertEqual("/home/foo", normpath("/home/bar/../foo")),
    ?assertEqual("/etc/foo", normpath("/home/bar/../../etc/foo")),
    ?assertEqual("etc/foo", normpath("/home/bar/../../../etc/foo")).
-endif.



