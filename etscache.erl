
%%-------------------------------------------------------------------
%%
%% File:      etscache.erl
%%
%% @author    Ricardo Gonçalves <tome.wave@gmail.com>
%%
%% @copyright 2011 Ricardo Tomé Gonçalves 
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

-author('Ricardo Tome Goncalves <tome@di.uminho.pt>').

-export([new/1, put_new/3, update/3, get/3, destroy/1]).

-type key() :: binary().
-type value() :: binary().


%% Container for the cache
%% It has 2 ets tables, and a fixed maximum size for the cache
-record(cache, {
	maxsize :: integer(),
	table, % :: tid(),
	itable % :: tid()
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
	Table = ets:new(table, [set,private,{keypos,#r_table.key}]),
	ITable = ets:new(itable, [ordered_set,private,{keypos,#r_itable.ts}]),
	ets:insert(Table, {dummy, ?CACHE_SIZE, 0}),
	#cache{maxsize = Maxsize, table=Table, itable=ITable}.
	
	

%% Insert NEW data in cache 
put_new(Cache, Key, Value) when is_list(Value) ->
 	put_new(Cache, Key, list_to_binary(Value)); 
put_new(#cache{maxsize=Msize, table=Tab,itable=ITab}, Key, Value) 
        when is_binary(Value) ->
	[{_,?CACHE_SIZE, CurrentSize}] = ets:lookup(Tab, ?CACHE_SIZE),
	Time = timestamp(),
    %% insert data in primary table
	case ets:insert_new(Tab, #r_table{key = Key, ts = Time, value = Value}) of
        %%don't do anything if the key exists
		false ->  
			{error, "Key already exists"};
		true -> 
            %% test if max size of cache has been reached
            case CurrentSize >= Msize of
                %% there is sufficient size
                false ->
                    ets:update_counter(Tab, ?CACHE_SIZE, {3, 1});
                %% need to prune data
                true ->
                    %% get oldest timestamp
                    PrunedKey = ets:first(ITab),
                    %% get key of the oldest timestamp
                    [RIT] = ets:lookup(ITab, PrunedKey),
                    %% delete timestamp
                    ets:delete(ITab, PrunedKey),
                    %% delete key
                    ets:delete(Tab, RIT#r_itable.key)
            end,
			%% insert data in inverse table
			ets:insert(ITab, #r_itable{ts = Time, key = Key}),
            ok
	end;
put_new(_,_,_) ->
    throw("invalid value").



%% insert data
update(Cache, Key, Value) when is_list(Value) ->
 	update(Cache, Key, list_to_binary(Value)); 
update(#cache{table=Tab, itable=ITab}, Key, Value) ->
	Time = timestamp(),
	%% update data in primary table
    case ets:update_element(Tab, Key, [{#r_table.ts, Time}, {#r_table.value, Value}]) of 
        true -> 
	        %% delete timestamp for the key
	        1 = ets:select_delete(ITab,[{ #r_itable{ts='_', key='$1'}, [], 
                                      [{'==', '$1', Key}]}]),
	        true = ets:insert_new(ITab, #r_itable{ts = Time, key = Key}),
            ok;
        false ->
            not_found
    end.



%% Get data given the key
get(#cache{table=Tab}, Key, Timeout) ->
	case ets:lookup(Tab, Key) of
		[Rtable] ->
			Value = binary_to_list(Rtable#r_table.value),
			TimeStamp = timestamp(),
		 	CompareTime = Rtable#r_table.ts + Timeout * 1000,
			if 
				CompareTime >= TimeStamp ->
					{ok, Value};
				CompareTime < TimeStamp ->
					timed_out
			end;
		[] ->
			not_found
	end.

destroy(#cache{table=Tab, itable=ITab}) ->
    ets:delete(Tab),
    ets:delete(ITab).

% @private
timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	(Mega*1000000+Sec)*1000000+Micro.

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(BATCHSIZE, 10000).

etscache_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(C) ->
            {inparallel,
                [insert_unique_new_t(C, I * ?BATCHSIZE + 1, (I + 1) * ?BATCHSIZE)
                || I <- lists:seq(0, 20)] ++
                [insert_existing_new_t(C, I) || I <- lists:seq(0,20)] ++
                [
                    update_non_existing(C),
                    update_existing(C)
                ]
             }
        end
    }.

start() ->
    C = etscache:new(100000),
    % fill the cache
    [put_new(C, "kk" ++ integer_to_list(I), <<"">>) || I <- lists:seq(1, 100000)],
    C.

stop(C) ->
    destroy(C).

insert_unique_new_t(C, Start, End) ->
    R = [put_new(C, "iunt" ++ integer_to_list(I), <<"ssssss">>) || I <- lists:seq(Start, End)],
    E = lists:duplicate(End - Start + 1, ok),
    %?debugFmt("R: ~p~n E: ~p~n", [R, E]),
    [?_assertEqual(E, R)].

insert_existing_new_t(C, I) ->
    Key = "ient" ++ integer_to_list(I),
    R = [put_new(C, Key, <<"ssssss">>) || _I <- lists:seq(1, ?BATCHSIZE)],
    E = [ok | lists:duplicate(?BATCHSIZE - 1, {error,"Key already exists"})],
    [?_assertEqual(E, R)].

update_non_existing(C) ->
    E = not_found,
    R1 = update(C, non_existing_key, <<"value">>),
    R2 = update(C, non_existing_key, <<"value">>),
    [?_assertEqual(E, R1),
     ?_assertEqual(E, R2)].

update_existing(C) ->
    ok = put_new(C, update_existing, <<"123">>),
    {ok, "123"} =  get(C, update_existing, 10000),
    ok = update(C, update_existing, <<"456">>),
    R = get(C, update_existing, 10000),
    [ ?_assertEqual({ok, "456"}, R)].

-endif.
