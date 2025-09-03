# maps_in

An Erlang library to handle nested maps.

## Table of contents

- [General info](#general-info)
- [Usage](#usage)
    - [filter/3](#filter3)
    - [filtermap/3](#filtermap3-otp-240)
    - [find/3](#find3)
    - [fold/4](#fold4)
    - [foreach/3](#foreach3)
    - [get/2](#get2)
    - [get/3](#get3)
    - [keys/2](#keys2)
    - [is_key/3](#is_key3)
    - [iterator/2](#iterator2-otp-21)
    - [map/3](#map3)
    - [merge/3](#merge3)
    - [merge_with/4](#merge_with4-otp-240)
    - [put/3](#put3)
    - [remove/3](#remove3)
    - [size/2](#size2)
    - [take/3](#take3)
    - [to_list/2](#to_list2)
    - [update/3](#update3)
    - [update_with/3](#update_with3)
    - [update_with/4](#update_with4)
    - [values/2](#values2)
    - [with/3](#with3)
    - [without/3](#without3)
- [Build](#build)
- [Test](#test)

## General info

Erlang does not provide functions to handle nested maps, so this lib has this purpose and always uses a list of keys to manipulate maps.

## Usage

### filter/3

```erlang
1> Map = #{erlang => #{example => #{a => 2, b => 3, c => 4, "a" => 1, "b" => 2, "c" => 4}}}.
#{erlang =>
      #{example =>
            #{a => 2,b => 3,c => 4,"a" => 1,"b" => 2,"c" => 4}}}
2> Pred = fun(K, V) -> is_atom(K) andalso (V rem 2) =:= 0 end.
#Fun<erl_eval.41.3316493>
3> maps_in:filter([erlang, example], Pred, Map).
#{erlang => #{example => #{a => 2,c => 4}}}
```

### filtermap/3 (OTP 24.0)

The maps_in:filtermap/3 function updates a nested map by applying a Fun to each key-value pair at a specified nested path. It's designed to both transform the values and filter the elements of the inner map in a single pass. The function returns a new map with the changes applied.

The Fun can return one of two values for each key-value pair:

{true, NewValue}: This keeps the key and updates its value to NewValue in the resulting map.

boolean(): This acts as a filter. If the function returns true, the key-value pair is kept as-is. If it returns false, the pair is removed from the map.

```erlang
1> Map = #{erlang => #{example => #{k1 => 1, "k2" => 2, "k3" => 3}}}.
#{erlang => #{example => #{k1 => 1,"k2" => 2,"k3" => 3}}}
2> Fun = fun(K, V) when is_atom(K) -> {true, V * 2}; (_, V) -> (V rem 2) =:= 0 end.
#Fun<erl_eval.41.113135111>
3> maps_in:filtermap([erlang, example], Fun, Map).
#{erlang => #{example => #{k1 => 2,"k2" => 2}}}
```

### find/3

The maps_in:find/3 function attempts to find a key within a nested map at a specific path.

It returns {ok, Value} if the key is found, and error if the key does not exist at the specified path. It essentially performs the same operation as maps:find/2, but on a map nested inside another one.

```erlang
1> Map = #{erlang => #{creators => #{joe => "Armstrong", robert => "Virding", mike => "Williams"}}}.
#{erlang =>
      #{creators =>
            #{joe => "Armstrong",robert => "Virding",mike => "Williams"}}}
2> maps_in:find(joe, [erlang, creators], Map).
{ok,"Armstrong"}
3> maps_in:find(freddy, [erlang, creators], Map).
error
```
### fold/4

The maps_in:fold/4 function iterates over the key-value pairs of a nested map at a specified path, accumulating a single value. It's used to reduce the map's contents to a single result, similar to how lists:foldl/3 works for lists. You provide an initial accumulator value and a function that takes a key, value, and the current accumulator to produce a new accumulator value.

```erlang
1> Map = #{fruits => #{apples => 5, oranges => 3, bananas => 2}, vegetables => #{carrots => 4, lettuce => 1}}.
#{fruits => #{apples => 5,oranges => 3,bananas => 2},
  vegetables => #{carrots => 4,lettuce => 1}}
2> SumFruitsFun = fun(_Key, Value, Acc) -> Acc + Value end.
#Fun<erl_eval.40.113135111>
3> maps_in:fold(SumFruitsFun, 0, [fruits], Map).
10
```

### foreach/3

The maps_in:foreach/3 function iterates over the key-value pairs of a nested map. It applies a given function to each pair for its side effects, and it always returns ok. This function is useful when you need to perform an action on each element of a nested map without accumulating a new value or modifying the map itself.

```erlang
1> Map = #{'users' => #{'user-1' => #{name => "Alice"}, 'user-2' => #{name => "Bob"}}}.
#{users =>
      #{'user-1' => #{name => "Alice"},
        'user-2' => #{name => "Bob"}}}
2> Fun = fun(Key, Value) -> io:format("User Key: ~p, Details: ~p~n", [Key, Value]) end.
#Fun<erl_eval.41.113135111>
3>  maps_in:foreach(Fun, [users], Map).
User Key: 'user-1', Details: #{name => "Alice"}
User Key: 'user-2', Details: #{name => "Bob"}

```

### get/2

```erlang
1> Map = #{my => #{nested => map}}.
#{my => #{nested => map}}
2> maps_in:get([my, nested], Map).
map
```

### get/3

```erlang
1> Map = #{my => #{nested => map}}.
#{my => #{nested => map}}
2> maps_in:get([my, unknown_key], Map, default).
default
```

### keys/2

The maps_in:keys/2 function returns a list of all the keys from a nested map located at a specified path. This is useful for getting a list of all the available keys at a given level within a complex nested map, allowing you to iterate over them or check for their presence.

```erlang
1> Map = #{products => #{fruits => #{apples => 5, oranges => 3}}}.
#{products => #{fruits => #{apples => 5,oranges => 3}}}
2> maps_in:keys([products, fruits], Map).
[apples,oranges]
```

### is_key/3

```erlang
1> Map = #{products => #{fruits => #{apples => 5, oranges => 3}}}.
#{products => #{fruits => #{apples => 5,oranges => 3}}}
2> maps_in:is_key(apples, [products, fruits], Map).
true
3> maps_in:is_key(bananas, [products, fruits], Map).
```

### iterator/2 (OTP 21)

```erlang
1> Map = #{users => #{'user-1' => #{name => "Alice", age => 30}, 'user-2' => #{name => "Bob", age => 25}}}.
#{users =>
      #{'user-1' => #{name => "Alice",age => 30},
        'user-2' => #{name => "Bob",age => 25}}}
2> Iterator = maps_in:iterator([users], Map).
[0|
 #{'user-1' => #{name => "Alice",age => 30},
   'user-2' => #{name => "Bob",age => 25}}]
3> {Key1, Val1, Iterator2} = maps:next(Iterator).
{'user-1',#{name => "Alice",age => 30},
          {'user-2',#{name => "Bob",age => 25},none}}
4> {Key2, Val2, Iterator3} = maps:next(Iterator2).
{'user-2',#{name => "Bob",age => 25},none}
5> maps:next(Iterator3).
none
```

### map/3

```erlang
1>  Map = #{a => #{b => #{c => 1, d => 2}}}.
#{a => #{b => #{c => 1,d => 2}}}
2> Fun = fun(_Key, Value) -> Value * 2 end.
#Fun<erl_eval.41.113135111>
3> maps_in:map([a, b], Fun, Map).
#{a => #{b => #{c => 2,d => 4}}}
```

### merge/3

The maps_in:merge/3 function merges the first map Map1 into a nested map within the second map Map2 at the specified path. The function returns a new map with the combined values. If a key exists in both maps, the value from the first map Map1 is used.

```erlang
1> Map1 = #{e => 5}.
#{e => 5}
2> Map2 = #{a => #{b => #{c => 3, d => 4}}}.
#{a => #{b => #{c => 3,d => 4}}}
3> maps_in:merge(Map1, [a, b], Map2).
#{a => #{b => #{c => 3,d => 4,e => 5}}}
```

### merge_with/4 (OTP 24.0)

The maps_in:merge_with/4 function is similar to maps_in:merge/3, but it uses a Combiner function to handle keys that exist in both the source map and the nested target map. The Combiner function takes the key, the value from the source map, and the value from the nested target map, and returns the new value to be used in the merged map.

```erlang
1> Map1 = #{b => 10, d => 20}.
#{b => 10,d => 20}
2> Map2 = #{a => #{b => #{c => 3, d => 4}}}.
#{a => #{b => #{c => 3,d => 4}}}
3> Combiner = fun(_Key, V1, V2) -> V1 + V2 end.
#Fun<erl_eval.40.113135111>
4> maps_in:merge_with(Combiner, Map1, [a, b], Map2).
#{a => #{b => #{c => 3,b => 10,d => 24}}}
```

### put/3

```erlang
1> Map = #{my => #{more => #{deep => #{}}}}.
#{my => #{more => #{deep => #{}}}}
2> maps_in:put([my, more, deep], #{nested => map}, Map).
#{my => #{more => #{deep => #{nested => map}}}}
```

### remove/3


The maps_in:remove/3 function removes a key and its corresponding value from a nested map at the specified path. It returns a new map without the removed key-value pair. If the key does not exist, the function returns the map unchanged.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2}}}.
#{a => #{b => #{c => 1,d => 2}}}
2> maps_in:remove(c, [a, b], Map).
#{a => #{b => #{d => 2}}}
3> maps_in:remove(e, [a, b], Map).
#{a => #{b => #{c => 1,d => 2}}}
```

### size/2

The maps_in:size/2 function returns the number of key-value pairs in a nested map at the specified path.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2, e => 3}}}.
#{a => #{b => #{c => 1,d => 2,e => 3}}}
2> maps_in:size([a, b], Map).
3
```

### take/3

The maps_in:take/3 function removes a key from a nested map at a given path and returns a tuple containing the value and the new map. If the key is not found, it returns error.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2}}}.
#{a => #{b => #{c => 1,d => 2}}}
2> maps_in:take(c, [a, b], Map).
{1,#{d => 2}}
3> maps_in:take(e, [a, b], Map).
error
```

### to_list/2

The maps_in:to_list/2 function converts a nested map at a specified path into a list of {Key, Value} tuples.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2}}}.
#{a => #{b => #{c => 1,d => 2}}}
2> maps_in:to_list([a, b], Map).
[{c,1},{d,2}]
```

### update/3

The maps_in:update/3 function updates the value of a key in a nested map. It takes a path, a new value, and the map. The function will raise an exception if the key does not exist.

```erlang
1> Map = #{my => #{more => #{deep => #{}}}}.
#{my => #{more => #{deep => #{}}}}
2> maps_in:update([my, unknown_key], error, Map).
** exception error: bad key: unknown_key
3> maps_in:update([my, more, deep], #{nested => map}, Map).
#{my => #{more => #{deep => #{nested => map}}}}
```

### update_with/3

The maps_in:update_with/3 function updates the value of a key in a nested map by applying a function to the existing value. It takes a path, a function, and the map. The function will raise an exception if the key does not exist.

```erlang
1> Map = #{someone => #{age => 17}}.
#{someone => #{age => 17}}
2> maps_in:update_with([someone, age], fun(Age) -> Age + 1 end, Map).
#{someone => #{age => 18}}
```

### update_with/4


The maps_in:update_with/4 function updates the value of a key in a nested map at a given path. If the key exists, the Fun is applied to the old value to produce a new value. If the key does not exist, the Init value is inserted.

```erlang
1> Map = #{a => #{b => #{c => 1}}}.
#{a => #{b => #{c => 1}}}
2> maps_in:update_with([a, b, c], fun(Val) -> Val + 1 end, 10, Map).
#{a => #{b => #{c => 2}}}
3> maps_in:update_with([a, b, d], fun(Val) -> Val + 1 end, 10, Map).
#{a => #{b => #{c => 1,d => 10}}}
```

### values/2

The maps_in:values/2 function returns a list of all the values from a nested map at the specified path. The order of the values in the returned list is not guaranteed.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2}}}.
#{a => #{b => #{c => 1,d => 2}}}
2> maps_in:values([a, b], Map).
[1,2]
```

### with/3


The maps_in:with/3 function returns a new map containing only the key-value pairs from a nested map that are specified in the Keys list.

```erlang
1>  Map = #{a => #{b => #{c => 1, d => 2, e => 3}}}.
#{a => #{b => #{c => 1,d => 2,e => 3}}}
2> maps_in:with([c, e], [a, b], Map).
#{c => 1,e => 3}
```

### without/3

The maps_in:without/3 function returns a new map that contains all the key-value pairs from a nested map, except for those specified in the Keys list.

```erlang
1> Map = #{a => #{b => #{c => 1, d => 2, e => 3}}}.
#{a => #{b => #{c => 1,d => 2,e => 3}}}
2> maps_in:without([c, e], [a, b], Map).
#{d => 2}
```

## Build

    $ rebar3 compile

## Test

    $ rebar3 eunit
