-module(maps_in).

-export([get_in/2, put_in/3, update_in/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_in(Path, Map) -> Value when
    Path :: [term()],
    Map :: map(),
    Value :: term().

get_in([Key], Map) ->
    maps:get(Key, Map);
get_in([Key | Path], Map) ->
    get_in(Path, maps:get(Key, Map)).

-spec put_in(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put_in([Key], Value, Map) ->
    maps:put(Key, Value, Map);
put_in([Key | Path], Value, Map) ->
    put_in(Path, Value, maps:get(Key, Map)).

-spec update_in(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

update_in([Key], Value, Map) ->
    maps:update(Key, Value, Map);
update_in([Key | Path], Value, Map) ->
    update_in(Path, Value, maps:get(Key, Map)).

-ifdef(TEST).

the_movie() ->
    #{
        joe => #{name => "Joe Armstrong", msg => "Hello, Robert"},
        robert => #{name => "Robert Virding", msg => "Hello, Mike"},
        mike => #{msg => "Hello, Robert and Joe"}
    }.

get_in_test() ->
    [
        ?assertEqual(#{name => "Joe Armstrong", msg => "Hello, Robert"},
                     get_in([joe], the_movie())),
        ?assertEqual("Hello, Mike",
                     get_in([robert, msg], the_movie())),
        ?assertError({badkey, name},
                     get_in([mike, name], the_movie())),
        ?assertError({badmap, "Hello, Robert and Joe"},
                     get_in([mike, msg, hello], the_movie()))
    ].

put_in_test() ->
    [
        ?assertEqual(#{name => "Mike Williams", msg => "Hello, Robert and Joe"},
                     put_in([mike, name], "Mike Williams", the_movie())),
        ?assertEqual(#{erlang => "The Movie"},
                     put_in([erlang], "The Movie", #{}))
    ].

update_in_test() ->
    [
        ?assertError({badkey, erlang},
                     update_in([erlang], "The Movie", #{})),
        ?assertEqual(#{erlang => "The Movie"},
                     update_in([erlang], "The Movie", #{erlang => ""}))
    ].

-endif.
