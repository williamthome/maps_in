%%%-----------------------------------------------------------------------------
%%% @doc Module to handle nested maps.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @todo improve docs and tests
%%% @end
%%%-----------------------------------------------------------------------------
-module(maps_in).

-export([get/2, get/3, get_and_update/3, is_key/3, keys/2, map/3, put/3,
         remove/3, size/2, update/3]).

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
%% @doc get_and_update/3.
%% @end
%%------------------------------------------------------------------------------
-spec get_and_update(Path, Fun, Map1) -> Map2 when
    Path :: [term()],
    Fun :: fun((term()) -> term()),
    Map1 :: map(),
    Map2 :: map().

get_and_update([Key], Fun, Map) when is_function(Fun, 1) ->
    maps:update(Key, Fun(maps:get(Key, Map)), Map);
get_and_update([Key | Path], Fun, Map) when is_function(Fun, 1) ->
    maps:update(Key, get_and_update(Path, Fun, maps:get(Key, Map, #{})), Map).

%%------------------------------------------------------------------------------
%% @doc is_key/3.
%% @end
%%------------------------------------------------------------------------------
-spec is_key(Key, Path, Map) -> boolean() when
    Key :: term(),
    Path :: [term()],
    Map :: map().

is_key(Key, Path, Map) ->
    lists:member(Key, keys(Path, Map)).

%%------------------------------------------------------------------------------
%% @doc keys/2.
%% @end
%%------------------------------------------------------------------------------
-spec keys(Path, Map) -> Keys when
    Path :: [term()],
    Map :: map(),
    Keys :: [term()].

keys(Path, Map) ->
    maps:keys(get(Path, Map)).

%%------------------------------------------------------------------------------
%% @doc map/3.
%% @end
%%------------------------------------------------------------------------------
-spec map(Path, Fun, Map1) -> Map2 when
    Path :: [term()],
    Fun :: fun((term(), term()) -> term()),
    Map1 :: map(),
    Map2 :: map().

map([Key], Fun, Map) when is_function(Fun, 2) ->
    maps:update(Key, maps:map(Fun, maps:get(Key, Map)), Map);
map([Key | Path], Fun, Map) when is_function(Fun, 2) ->
    maps:update(Key, map(Path, Fun, maps:get(Key, Map, #{})), Map).

%%------------------------------------------------------------------------------
%% @doc put/3.
%% @end
%%------------------------------------------------------------------------------
-spec put(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put([Key | Path], Value, Map) ->
    maps:put(Key, put(Path, Value, maps:get(Key, Map, #{})), Map);
put([], Map, _) ->
    Map.

%%------------------------------------------------------------------------------
%% @doc remove/3.
%% @end
%%------------------------------------------------------------------------------
-spec remove(Key, Path, Map1) -> Map2 when
    Key :: term(),
    Path :: [term()],
    Map1 :: map(),
    Map2 :: map().

remove(KeyRem, [Key], Map) ->
    maps:update(Key, maps:remove(KeyRem, maps:get(Key, Map)), Map);
remove(KeyRem, [Key | Path], Map) ->
    maps:update(Key, remove(KeyRem, Path, maps:get(Key, Map, #{})), Map).

%%------------------------------------------------------------------------------
%% @doc size/2.
%% @end
%%------------------------------------------------------------------------------
-spec size(Path, Map) -> non_neg_integer() when
    Path :: [term()],
    Map :: map().

size(Path, Map) ->
    maps:size(get(Path, Map)).

%%------------------------------------------------------------------------------
%% @doc update/3.
%% @end
%%------------------------------------------------------------------------------
-spec update(Path, Value, Map1) -> Map2 when
    Path :: [term()],
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

update([Key | Path], Value, Map) ->
    maps:update(Key, update(Path, Value, maps:get(Key, Map, #{})), Map);
update([], Map, _) ->
    Map.

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

erlang_creators() ->
    #{erlang => #{creators => #{joe => "Joe",
                                robert => "Robert",
                                mike => "Mike"}}}.

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

get_and_update_3_test() ->
    [?assertError({badkey, erlang},
                  get_and_update([erlang], fun(_) -> error end, #{})),
     ?assertError(function_clause,
                  get_and_update([erlang], "The Movie", #{erlang => ""})),
     ?assertEqual(#{erlang => "The Movie"},
                  get_and_update([erlang],
                                 fun("") -> "The Movie" end,
                                 #{erlang => ""})),
     ?assertEqual(#{erlang => #{the => #{movie => "The Movie"}}},
                  get_and_update([erlang, the, movie],
                                 fun("") -> "The Movie" end,
                                 #{erlang => #{the => #{movie => ""}}}))].

keys_2_test() ->
    Keys = keys([erlang, creators], erlang_creators()),
    ?assert(lists:all(fun(K) -> lists:member(K, Keys) end, [joe, robert, mike])).

is_key_3_test() ->
    [?assert(is_key(joe, [erlang, creators], erlang_creators())),
     ?assertNot(is_key(jose, [erlang, creators], erlang_creators()))].

map_3_test() ->
    Fun = fun(joe, Joe) -> Joe ++ " Armstrong";
             (robert, Robert) -> Robert ++ " Virding";
             (mike, Mike) -> Mike ++ " Williams" end,
    ?assertEqual(#{erlang => #{creators => #{joe => "Joe Armstrong",
                                             robert => "Robert Virding",
                                             mike => "Mike Williams"}}},
                 map([erlang, creators], Fun, erlang_creators())).

put_3_test() ->
    [?assertEqual(#{joe => #{name => "Joe Armstrong", msg => "Hello, Robert"},
                    robert => #{name => "Robert Virding", msg => "Hello, Mike"},
                    mike => #{name => "Mike Williams", msg => "Hello, Robert and Joe"}},
                  put([mike, name], "Mike Williams", the_movie())),
     ?assertEqual(#{erlang => "The Movie"},
                  put([erlang], "The Movie", #{}))].

remove_3_test() ->
    Map = #{this => #{should => #{be => #{ok => ok, removed => removed}}}},
    ?assertEqual(#{this => #{should => #{be => #{ok => ok}}}},
                 remove(removed, [this, should, be], Map)).

size_2_test() ->
    ?assertEqual(3, size([erlang, creators], erlang_creators())).

update_3_test() ->
    [?assertError({badkey, erlang},
                  update([erlang], "The Movie", #{})),
     ?assertEqual(#{erlang => "The Movie"},
                  update([erlang], "The Movie", #{erlang => ""}))].

-endif.
