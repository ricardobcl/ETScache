# ETS cache


ets\_cache is a very(!) simple, self-contained memory cache, using only [ETS (Erlang Term Storage)][ETS]. You create a cache that has limit of maximum number of elements. When this limit is exceed, the oldest element is removed.

It has the following functions:

```Erlang
new (max_size) -> ets_cache()
destroy (ets_cache) -> true

put (ets_cache, key, value) -> true
put (ets_cache, key, value, timestamp) -> true

get (ets_cache, key) -> {ok, value} | not_found
get (ets_cache, key, timeout) -> {ok, value} | not_found | expired
```

It is design to perform rapidly: 

* `get` is constant **O(1)**;
* `put` is logarithmic **O(log(N))**;

PS: For performance, a good rule is to use binaries as much as possible for representing strings and large blocks of untyped memory.

### Author

[Ricardo Tomé Gonçalves][ricardo github]
<tome.wave@gmail.com>

### Contributions

[Ali Yakamercan][Ali github]

### How To Use

```Erlang
% Creates a new cache with a maximum of 2 elements in cache
C = ets_cache:new(2),

% Add key/values
ets_cache:put(C, key1, v1),
ets_cache:put(C, key2, v2),
ets_cache:put(C, key3, v3),
ets_cache:put(C, key3, v33), 

% Get values
not_found = ets_cache:get(C, key1),
{ok, v2}  = ets_cache:get(C, key2),
{ok, v33} = ets_cache:get(C, key3),
expired   = ets_cache:get(C, key3, 0),
```

### Test It

```bash
$ erlc -DTEST ets_cache.erl; erl -eval "eunit:test(ets_cache)"
```

### TODO

* Look at Judy C implemention http://judy.sourceforge.net/doc/index.html
* Benchmarks!!

[ricardo github]: https://github.com/ricardobcl
[Ali github]: https://github.com/aliyakamercan
[ETS]: http://www.erlang.org/doc/man/ets.html
