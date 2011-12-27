
%%-------------------------------------------------------------------
%%
%% File:      etscache.erl
%%
%% @author    Ricardo Gonçalves <tome.wave@gmail.com>
%%
%% @copyright 2011 Ricardo Gonçalves 
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc  
%%	ETScache is very(!) simple in-memory cache, using ETS tables in Erlang. 
%%  You can create a cache with a maximum number of elements in it, and when 
%%  this limit is exceed, the oldest element is eliminated.
%% @end  
%%
%%-------------------------------------------------------------------


-module(etscache).
-export([new/1, put_new/3, update/3, get/2, test/0]).

-type key() :: binary().
-type value() :: binary().


%% Container for the cache, a.k.a. etscache()
%% It has 2 ets tables, and a fixed maximum size for the cache
-record(cache, {
	maxsize :: integer(),
%	size :: integer(),
	table :: tid(),
	itable :: tid()
}).

%% Table with the key, value and timestamp
-record(r_table, {
	key :: key(),
	ts :: integer(),
	value :: value()
}).


%% Inverted table, ordered by timestamp, so we can always know the oldest key
-record(r_itable, {
	ts :: integer(),
	key :: key()
}).

%% the key where is stored the current cache size
-define(CACHE_SIZE, cache_size).

%% Create a new cache with a mixumim size
new(Maxsize) -> 
	Table = ets:new(table, [set,private,{keypos,2}]),
	ITable = ets:new(itable, [ordered_set,private,{keypos,2}]),
	ets:insert(Table, {dummy, ?CACHE_SIZE, 0}),
	#cache{maxsize = Maxsize, table=Table, itable=ITable}.
	
	

%% Insert NEW data in cache 
put_new(_Cache=#cache{maxsize=Msize, table=Tab,itable=ITab}, Key, Value) ->
	Bin = list_to_binary(Value),
	[{_,?CACHE_SIZE, CurrentSize}] = ets:lookup(Tab, ?CACHE_SIZE),
    %%io:format("Current size ~p~n", [CurrentSize]),

	Size = 1, 	%% Size = size(Bin),
	Time = timestamp(),
	%% test if max size of cache has been reached
	case CurrentSize >= Msize of
		%% there is sufficient size
		false ->
			%%io:format("there is sufficient size~n", []),
			ets:update_counter(Tab, ?CACHE_SIZE, Size);
			%Cache#cache{size = Tsize + Size};
		%% need to prune data
		true ->
			%%io:format("there is NOT sufficient size~n", []),
			%% get oldest timestamp
			PrunedKey = ets:first(ITab),
			%% get key of the oldest timestamp
			[RIT] = ets:lookup(ITab, PrunedKey),
			%% delete timestamp
			ets:delete(ITab, PrunedKey),
			%% delete key
			%%io:format("Deleting:~p~n", [RIT#r_itable.key]),
			ets:delete(Tab, RIT#r_itable.key)
	end,
	%% insert data in primary table
	case ets:insert_new(Tab, #r_table{key = Key, ts = Time, value = Bin}) of
		false -> 
			{error, "Key already exists"};
		%% insert data in inverse table
		true -> 
			%io:format("inserting ~p~nAnd ", [#r_table{key = Key, ts = Time, value = Bin}]),
			ets:insert(ITab, #r_itable{ts = Time, key = Key}),
			%io:format("~p~n", [#r_itable{ts = Time, key = Key}]),
			ok
	end.



%% insert (probably not new) data
update(_Cache=#cache{table=Tab, itable=ITab}, Key, Value) ->
	Bin = list_to_binary(Value),
	Time = timestamp(),
	%% insert data in primary table
	ets:insert(Tab, #r_table{key = Key, ts = Time, value = Bin}),
	%% delete timestamp for the key
	_Num = ets:select_delete(ITab,[{ #r_itable{ts='_', key='$1'}, [], [{'==', '$1', Key}]}]),
	%%io:format("Num of deletes = ~p~n", [Num]),
	%% insert data in inverse table
	ets:insert(ITab, #r_itable{ts = Time, key = Key}).



%% Get data given the key
get(#cache{table=Tab}, Key) ->
    case ets:lookup(Tab, Key) of
        [Rtable] ->
			Value = binary_to_list(Rtable#r_table.value),
			%%io:format("We found it, key= ~p value=~p~n", [Key,Value]),
			{ok, Value};
        [] ->
			%%io:format("No person with ID = ~p~n", [Key]),
			not_found
    end.


% @private
timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	(Mega*1000000+Sec)*1000000+Micro.






%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
% erlc etscache.erl 
% erl -noshell -s etscache test -s init stop

test() ->
	C = etscache:new(3),
	TabList = ets:tab2list(C#cache.table),
	io:format("~n~nLista: ~p~n", [TabList]),
	ok = etscache:put_new(C, key1, "v1"), 
	{error,_} = etscache:put_new(C, key1, "cenas"), 
	ok = etscache:put_new(C, key2, "v2"), 
	ok = etscache:put_new(C, key3, "v3"), 
	ok = etscache:put_new(C, key4, "v4"), 
	ok = etscache:put_new(C, key5, "v5"), 
	ok = etscache:put_new(C, key6, "v6"), 
	ok = etscache:put_new(C, key7, "v7"), 
	ok = etscache:put_new(C, key2, "v2"), 
	ok = etscache:put_new(C, key3, "v3"), 
	etscache:update(C, key7, "v77"), 
	ok = etscache:put_new(C, key9, "v9"), 
	etscache:update(C, key9, "v99"), 
	etscache:update(C, key9, "v999"), 
	etscache:update(C, key9, "v9999"), 
	etscache:update(C, key9, "v99999"), 
	etscache:update(C, key3, "v3333"), 
	ok = etscache:put_new(C, key10, "v100"), 
	not_found = etscache:get(C, key99),
	TabList2 = ets:tab2list(C#cache.table),
	TabList3 = ets:tab2list(C#cache.itable),
	io:format("~n~nLista: ~p~n~nAnd ~p~n", [TabList2,TabList3]),
	etscache:get(C, key10),
	etscache:get(C, key9),
	etscache:get(C, key100).
	
	
	
	
	
	
	