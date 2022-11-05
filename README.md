# maps_in

An Erlang library to handle nested maps.

## Table of contents

* [General info](#general-info)
* [Usage](#usage)
* [Build](#build)
* [Test](#test)

## General info

Erlang does not provide functions to handle nested maps, so this lib has this purpose and always uses a list of keys to manipulate maps.

## Usage

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

### put/3

```erlang
1> Map = #{my => #{more => #{deep => #{}}}}.
#{my => #{more => #{deep => #{}}}}
2> maps_in:put([my, more, deep], #{nested => map}, Map).
#{my => #{more => #{deep => #{nested => map}}}}
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

### get_and_update/3

```erlang
1> Map = #{someone => #{age => 17}}.
#{someone => #{age => 17}}
2> maps_in:get_and_update([someone, age], fun(Age) -> Age + 1 end, Map).
#{someone => #{age => 18}}
```

## Build

    $ rebar3 compile

## Test

    $ rebar3 eunit
