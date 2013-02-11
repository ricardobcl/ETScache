# ETS cache


ETScache is a very(!) simple, self-contained memory cache, using only ETS (Erlang term storage). You can create a cache with a maximum number of elements in it, and when this limit is exceed, the oldest element is eliminated.

It has the following functions

* new (max_size) -> etscache()
* put\_new (etscache, key, value) -> ok | already_exists
* update (etscache, key, value) -> ok | not_found
* get (etscache, key, timeout) -> {ok, value} | not_found | expired

It is design to perform rapidly: 
* *Get* is constant **O(1)**; 
* *Put\_New* is constant **O(1)**; 
* *Update* is linear with the number of elements in cache **O(N)**.

### Author

[Ricardo Tomé Gonçalves][ricardo github]
<tome.wave@gmail.com>

### Contributions

[Ali Yakamercan][Ali github]

### How To Use

```Erlang
% Create a new cache with the maximum number of elements in cache
C = etscache:new(128),

% Add a key/value
ok = etscache:put_new(C, key1, "v1"), 

% put_new only adds new values
{error,_} = etscache:put_new(C, key1, "v2"), 

% Update an existing value
etscache:update(C, key1, "v11"), 

% Get the value for some key
Milliseconds = 10000,
case etscache:get(C, key1, Milliseconds) of
    not_found -> io:format("Value not Found!~n");
    {ok, Value} -> io:format("Value found -> ~p~n", [Value]);
    expired -> io:format("Value is Expired!~n")
end.
```

### Test It

```bash
$ erlc -DTEST etscache.erl 
$ erl -eval "eunit:test(etscache)"
```

### TODO

* Look at Judy C implemention http://judy.sourceforge.net/doc/index.html
* Benchmarks!!

[ricardo github]: https://github.com/ricardobcl
[Ali github]: https://github.com/aliyakamercan