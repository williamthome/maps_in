-module(maps_in).

-export([get_in/2]).

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

-ifdef(TEST).

the_movie() ->
    #{
        joe => #{name => "Joe Armstrong", msg => "Hello Robert"},
        robert => #{name => "Robert Virding", msg => "Hello Mike"},
        mike => #{msg => "Hello Robert and Joe"}
    }.

get_in_test() ->
    [
        ?assertEqual(#{name => "Joe Armstrong", msg => "Hello Robert"}, get_in([joe], the_movie())),
        ?assertEqual("Hello Mike", get_in([robert, msg], the_movie())),
        ?assertError({badkey, name}, get_in([mike, name], the_movie())),
        ?assertError({badmap, "Hello Robert and Joe"}, get_in([mike, msg, hello], the_movie()))
    ].

-endif.
