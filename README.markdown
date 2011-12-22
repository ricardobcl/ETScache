Cherly
=======

ETScache (sher-lee) is an in-VM caching library for Erlang.  It is implemented as a linked in driver and the main lookup mechanism is Judy arrays.  There's almost no copying or reallocation of binary data involved, so cherly should be blindingly fast.  Cherly is designed for the needs of Dynomite, but it can live as a caching library in its own right.

Surely you can't be serious?

How To Use
=======

		% Create a new cache
        C = etscache:new(128),      %start takes 1 argument, the maximum number of elememts in cache

		% Add a key/value
		ok = etscache:put_new(C, key1, "v1"), 
		
		% put_new only adds new values
		{error,_,_} = etscache:put_new(C, key1, "cenas"), 
		
		% Update an existing value
		etscache:update(C, key1, "v11"), 
		
		% Get the value for some key
        case etscache:get(C, key1) of
          not_found -> io:format("Value not Found!~n");
          {ok, Value} -> io:format("Value found -> ~p~n", [Value])
        end.
