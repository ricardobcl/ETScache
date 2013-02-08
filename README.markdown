ETScache
=======

ETScache is very(!) simple, self-contained memory cache, using only ETS tables in Erlang. You can create a cache with a maximum number of elements in it, and when this limit is exceed, the oldest element is eliminated.

It has the following functions

*	new (max_size) -> etscache
*	put_new (etscache, key, value) -> ok ; {error, "Already exists!"}
*	update (etscache, key, value) -> ok | not_found
*	get (etscache, key, timeout) -> {ok, value} | not_found | expired


It is design to perform rapidly: _Get_ is constant **_O(1)_**; _Put\_New_ is constant **_O(1)_**; Update is linear with the number os elements in cache **_O(N)_**.

Author
------

Ricardo Gonçalves <tome.wave@gmail.com>

How To Use
----------

		% Create a new cache
        C = etscache:new(128),      %start takes 1 argument, the maximum number of elememts in cache

		% Add a key/value
		ok = etscache:put_new(C, key1, "v1"), 
		
		% put_new only adds new values
		{error,_} = etscache:put_new(C, key1, "cenas"), 
		
		% Update an existing value
		etscache:update(C, key1, "v11"), 
		
		% Get the value for some key
        Milliseconds = 10000
        case etscache:get(C, key1, Milliseconds) of
          not_found -> io:format("Value not Found!~n");
          {ok, Value} -> io:format("Value found -> ~p~n", [Value]);
          expired -> io:format("Value is Expired!~n");
        end.


Test It
-------

Function _test_ gives and example of a possible run, and output the cache in the end, to check its state.

	~$ cd "ETScacheFolder"
	~$ erlc -DTEST etscache.erl 
	~$ erl -eval "eunit:test(etscache)"
	
	
TODO
----

*	Add proper testing framework. Look at <http://etrepum.github.com/erl_testing_2011/>
*	Look at Judy C implemention http://judy.sourceforge.net/doc/index.html
*	Benchmarks!!
