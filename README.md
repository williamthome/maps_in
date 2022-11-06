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

```erlang
<!-- TODO -->
```

### find/3

```erlang
<!-- TODO -->
```

### fold/4

```erlang
<!-- TODO -->
```

### foreach/3

```erlang
<!-- TODO -->
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

```erlang
<!-- TODO -->
```

### is_key/3

```erlang
<!-- TODO -->
```

### iterator/2 (OTP 21)

```erlang
<!-- TODO -->
```

### map/3

```erlang
<!-- TODO -->
```

### merge/3

```erlang
<!-- TODO -->
```

### merge_with/4 (OTP 24.0)

```erlang
<!-- TODO -->
```

### put/3

```erlang
1> Map = #{my => #{more => #{deep => #{}}}}.
#{my => #{more => #{deep => #{}}}}
2> maps_in:put([my, more, deep], #{nested => map}, Map).
#{my => #{more => #{deep => #{nested => map}}}}
```

### remove/3

```erlang
<!-- TODO -->
```

### size/2

```erlang
<!-- TODO -->
```

### take/3

```erlang
<!-- TODO -->
```

### to_list/2

```erlang
<!-- TODO -->
```

### update/3

```erlang
1> Map = #{my => #{more => #{deep => #{}}}}.
#{my => #{more => #{deep => #{}}}}
2> maps_in:update([my, unknown_key], error, Map).
** exception error: bad key: unknown_key
3> maps_in:update([my, more, deep], #{nested => map}, Map).
#{my => #{more => #{deep => #{nested => map}}}}
```

### update_with/3

```erlang
1> Map = #{someone => #{age => 17}}.
#{someone => #{age => 17}}
2> maps_in:update_with([someone, age], fun(Age) -> Age + 1 end, Map).
#{someone => #{age => 18}}
```

### update_with/4

```erlang
<!-- TODO -->
```

### values/2

```erlang
<!-- TODO -->
```

### with/3

```erlang
<!-- TODO -->
```

### without/3

```erlang
<!-- TODO -->
```

## Build

    $ rebar3 compile

## Test

    $ rebar3 eunit
