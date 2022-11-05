%%%-----------------------------------------------------------------------------
%%% @doc Module to handle nested maps.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @todo docs
%%% @end
%%%-----------------------------------------------------------------------------
-module(maps_in).

-export([get/2, get/3, put/3, update/3, get_and_update/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc get/2.
%% @end
%%------------------------------------------------------------------------------
-spec get(Path, Map) -> Value when
    Path :: [term()],
    Map :: map(),
    Value :: term().

get([Key], Map) ->
    maps:get(Key, Map);
get([Key | Path], Map) ->
    get(Path, maps:get(Key, Map)).

%%------------------------------------------------------------------------------
%% @doc get/3.
%% @end
%%------------------------------------------------------------------------------
-spec get(Path, Map, Default) -> Value | Default when
    Path :: [term()],
    Map :: map(),
    Default :: term(),
    Value :: term().

get([Key], Map, Default) ->
    maps:get(Key, Map, Default);
get([Key | Path], Map, Default) ->
    get(Path, maps:get(Key, Map), Default).

%%------------------------------------------------------------------------------
%% @doc put/3.
%% @end
%%------------------------------------------------------------------------------
-spec put(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put([Key], Value, Map) ->
    maps:put(Key, Value, Map);
put([Key | Path], Value, Map) ->
    put(Path, Value, maps:get(Key, Map)).

%%------------------------------------------------------------------------------
%% @doc update/3.
%% @end
%%------------------------------------------------------------------------------
-spec update(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

update([Key], Value, Map) ->
    maps:update(Key, Value, Map);
update([Key | Path], Value, Map) ->
    update(Path, Value, maps:get(Key, Map)).

%%------------------------------------------------------------------------------
%% @doc get_and_update/3.
%% @end
%%------------------------------------------------------------------------------
-spec get_and_update(Path, Fun, Map1) -> Map2 when
    Path :: [term()],
    Fun :: fun((term()) -> term()),
    Map1 :: map(),
    Map2 :: map().

get_and_update([Key], Fun, Map) when is_function(Fun, 1) ->
    Value = maps:get(Key, Map),
    maps:update(Key, Fun(Value), Map);
get_and_update([Key | Path], Fun, Map) ->
    get_and_update(Path, Fun, maps:get(Key, Map)).

%%%=============================================================================
%%% Test
%%%=============================================================================

-ifdef(TEST).

%%%-----------------------------------------------------------------------------
%%% Support
%%%-----------------------------------------------------------------------------

the_movie() ->
    #{joe => #{name => "Joe Armstrong", msg => "Hello, Robert"},
      robert => #{name => "Robert Virding", msg => "Hello, Mike"},
      mike => #{msg => "Hello, Robert and Joe"}}.

%%%-----------------------------------------------------------------------------
%%% Unit tests
%%%-----------------------------------------------------------------------------

get_2_test() ->
    [?assertEqual(#{name => "Joe Armstrong", msg => "Hello, Robert"},
                  get([joe], the_movie())),
     ?assertEqual("Hello, Mike",
                  get([robert, msg], the_movie())),
     ?assertError({badkey, name},
                  get([mike, name], the_movie())),
     ?assertError({badmap, "Hello, Robert and Joe"},
                  get([mike, msg, hello], the_movie()))].

get_3_test() ->
    [?assertEqual("Hello, Mike",
                  get([robert, msg], the_movie(), "Hello, Joe")),
     ?assertEqual("Mike Williams",
                  get([mike, name], the_movie(), "Mike Williams"))].

put_3_test() ->
    [?assertEqual(#{name => "Mike Williams", msg => "Hello, Robert and Joe"},
                  put([mike, name], "Mike Williams", the_movie())),
     ?assertEqual(#{erlang => "The Movie"},
                  put([erlang], "The Movie", #{}))].

update_3_test() ->
    [?assertError({badkey, erlang},
                  update([erlang], "The Movie", #{})),
     ?assertEqual(#{erlang => "The Movie"},
                  update([erlang], "The Movie", #{erlang => ""}))].

get_and_update_3_test() ->
    [?assertError({badkey, erlang},
                  get_and_update([erlang], "The Movie", #{})),
     ?assertError(function_clause,
                  get_and_update([erlang], "The Movie", #{erlang => ""})),
     ?assertEqual(#{erlang => "The Movie"},
                  get_and_update([erlang],
                                 fun("") -> "The Movie" end,
                                 #{erlang => ""}))].

-endif.
